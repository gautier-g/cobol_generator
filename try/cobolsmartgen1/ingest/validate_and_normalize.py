"""
Validation and normalization module for input specifications.
Reads input spec, validates against schema, normalizes types and casing.
"""

import json
import re
from datetime import datetime
from pathlib import Path

# Defaults communs appliqués si absents des specs (socle partagé entre projets)
COMMON_DEFAULTS = {
    "dialecte_cobol": "gnucobol",
    "sql_cible": "postgres",
    "nommage": {
        "variables_case": "UPPER_SNAKE",
        "programmes_case": "UPPER-KEBAB",
    },
    "cobol_format": {
        "line_format": "fixed",
        "indent_spaces": 7,
        "max_line_length": 72,
        "comment": {"column": 7, "prefix": "*"},
        "literal_quotes": "single",
        "paragraph_terminator": ".",
        "allow_source_format_directive": False,
    },
    "naming_rules": {
        "prefixes": {
            "working_storage": "WS-",
            "linkage": "LK-",
            "flags": "WS-",
        },
        "paragraph_style": "VERB-NOUN",
    },
    "sql": {
        "behavior": {
            "check_sqlcode": True,
            "check_sqlstate": True,
            "sqlcode_success": 0,
            "sqlcode_eof": 100,
            "rollback_on_error": False,
            "commit": {"mode": "end", "statement": "EXEC SQL COMMIT END-EXEC."},
            "disconnect": {"statement": "EXEC SQL DISCONNECT ALL END-EXEC."},
            "null_indicators": False,
            "cursor_commit_policy": "no_commit_while_open",
        },
        "formatting": {
            "max_line_length": 72,
            "host_vars_one_per_line": True,
            "split_long_exec_sql": True,
        },
        "connection": {
            "engine": "postgres",
            "method": "precompile",
            "source": "env",
            "env_vars": [],
            "env_set_method": [],
            "ocesql": {
                "start_call": "OCESQLStartSQL",
                "connect_call": "OCESQLConnect",
                "end_call": "OCESQLEndSQL",
            },
        },
    },
    "technique": {
        "cursor_naming": {"pattern": "C_<ENTITY_UPPER3>"},
        "eof_flag": {"name": "END-OF-FILE", "pic": "X", "initial_value": "N", "true_value": "Y", "false_value": "N"},
        "sqlca_required": True,
        "dal_call_pattern": {
            "description": "Programme DAL appele avec un code operation court (READ, SAVE, END).",
            "operation_param_name": "OPERATION",
            "operation_pic": "X(4)",
            "eof_param_name": "END-OF-FILE",
        },
    },
    "prompting": {
        "global_constraints": [
            "Reponds uniquement avec du code COBOL compilable, sans Markdown ni texte autour.",
            "La premiere ligne doit etre IDENTIFICATION DIVISION.",
            "Un seul PROGRAM-ID, pas de sous-programmes.",
        ],
        "forbidden_items": ["GO TO", "*>"],
        "global_directives": [
            "Utilise strictement les identifiants, messages, SQL et sequences definis dans la spec.",
            "N'invente aucune variable, paragraphe, message ou SQL.",
            "Respecte exactement les listes de WORKING-STORAGE, LINKAGE et USING.",
            "Connexion SQL: n'ecris jamais de valeurs en dur (hote/user/mdp/base), utilise uniquement les variables de la spec ou l'environnement externe.",
            "Toujours utiliser les terminateurs explicites END-IF / END-EVALUATE / END-PERFORM / END-EXEC.",
            "Pas de GO TO, pas de logique DAL dans LOGIC, pas de SQL hors DAL.",
            "Toute variable utilisee doit etre declaree (WORKING-STORAGE ou LINKAGE).",
            "REGLE ABSOLUE PARAGRAPHES: Le nom ET le point sur la MEME ligne. Format: CALCULATE-FEES. JAMAIS de ligne avec juste un point apres.",
            "REGLE ABSOLUE EXEC SQL INCLUDE: EXEC SQL INCLUDE SQLCA END-EXEC. (avec point final). Autres instructions COBOL sans point final.",
            "REGLE ABSOLUE QUALIFICATION: Sous-champs avec OF obligatoire. Format: AMOUNT-BRUT OF OPERATION-REC.",
            "Dans DATA DIVISION (WORKING-STORAGE/LINKAGE), toute ligne 01/05/77/88 avec PIC ou VALUE doit se terminer par un point. Regle non applicable en PROCEDURE DIVISION.",
        ],
        "generation_strategy": [
            "1) Construis d'abord le squelette COBOL complet (DIVISION/SECTION) dans l'ordre.",
            "2) Insere les blocs WORKING-STORAGE/LINKAGE exacts fournis par la spec.",
            "3) Ecris la PROCEDURE DIVISION en suivant le flow et les fonctions.",
            "4) REGLE CRITIQUE FERMETURE: Pour toute fonction ouverte (OPEN cursor, EVALUATE, IF, PERFORM), tu DOIS la fermer (CLOSE, END-EVALUATE, END-IF, END-PERFORM).",
            "5) Verifie la coherence: aucune variable/paragraphes inventes.",
        ],
        "formatting_guidelines": [
            "Format FIXED: instructions commencent en colonne 8 (prefixe de 7 espaces).",
            "Commentaires: '*' en colonne 7 (6 espaces + '*'), pas de commentaires '*>'.",
            "Ne pas utiliser '>>SOURCE FORMAT FREE'.",
            "PARAGRAPHES: Nom + point sur meme ligne. Format: MAIN-PROCESS. JAMAIS de point seul sur ligne.",
            "En DATA DIVISION, chaque ligne 01/05/77/88 avec PIC ou VALUE se termine par un point.",
            "Aucune ligne ne doit depasser 72 colonnes (format FIXED).",
            "Si une instruction depasse 72 colonnes, la scinder sur plusieurs lignes.",
            "Pour EXEC SQL FETCH/SELECT/UPDATE: mettre chaque variable hote sur sa propre ligne.",
            "Ne pas mettre de point en fin de DISPLAY/GOBACK/STOP RUN/CALL/MOVE/IF (sauf EXEC SQL INCLUDE SQLCA END-EXEC.).",
            "Utiliser des guillemets simples pour les messages DISPLAY.",
            "Respecter l'ordre: IDENTIFICATION -> ENVIRONMENT -> DATA -> PROCEDURE.",
            "Dans DATA: WORKING-STORAGE puis LINKAGE (si present).",
        ],
    },
}


def _deep_merge_defaults(spec: dict, defaults: dict) -> None:
    """Injecte les valeurs par défaut pour chaque clé absente (merge profond, non destructif)."""
    for k, v in defaults.items():
        if k not in spec or spec.get(k) is None:
            spec[k] = v
        elif isinstance(v, dict) and isinstance(spec.get(k), dict):
            _deep_merge_defaults(spec[k], v)

from typing import Dict, Any, List, Optional

from ..utils import fs, trace, typing_map
from . import diagram_parser



SYNONYMS_EXIG_TYPE = {
    # on respecte 'tech' tel quel; on garde d'autres alias si l'utilisateur les emploie
    'tech': 'tech',
    'technique': 'technique',
    'regle_metier': 'regle_metier',
    'business': 'business',
    'fonctionnelle': 'fonctionnelle',
    'non_fonctionnelle': 'non_fonctionnelle',
}

def run(input_path: str, config: Dict, out_dir: str) -> str:
    """
    Validate and normalize input specification.
    
    Args:
        input_path: Path to input specification file (YAML/JSON)
        config: Project configuration dictionary
        out_dir: Output directory path
        
    Returns:
        Path to normalized specification JSON file
        
    Raises:
        ValueError: If validation fails with aggregated error messages
    """
    # Ensure output directory exists
    out_path = Path(out_dir)
    out_path.mkdir(parents=True, exist_ok=True)
    
    # Load input specification
    if input_path.endswith('.json'):
        spec = fs.read_json(input_path)


    else:
        spec = fs.read_yaml(input_path)
    

    # Injecter les valeurs communes par défaut (socle) si absentes
    _deep_merge_defaults(spec, COMMON_DEFAULTS)

    # Harmoniser mode/source pour la connexion SQL
    conn = (spec.get("sql") or {}).get("connection")
    if isinstance(conn, dict):
        if "source" not in conn and conn.get("mode"):
            conn["source"] = conn.get("mode")
        if "mode" not in conn and conn.get("source"):
            conn["mode"] = conn.get("source")

    # Injecter les lignes WORKING-STORAGE de connexion (si definies dans la spec)
    conn_ws = None
    if isinstance(conn, dict):
        conn_ws = conn.get("working_storage_lines")
    if isinstance(conn_ws, list) and conn_ws:
        for prog in spec.get("programmes", []) or []:
            if (prog.get("layer") or "").lower() != "dal":
                continue
            ws_lines = prog.get("working_storage_lines", []) or []
            if not isinstance(ws_lines, list):
                continue
            for line in conn_ws:
                if line not in ws_lines:
                    ws_lines.append(line)
            prog["working_storage_lines"] = ws_lines


    # Harmonisation douce des types d'exigences (ne modifie pas la sémantique)
    for _ex in (spec.get('exigences') or []):
        t = _ex.get('type')
        if isinstance(t, str) and t in SYNONYMS_EXIG_TYPE:
            _ex['type'] = SYNONYMS_EXIG_TYPE[t]
    # Validate against schema
    errors = validate_spec(spec)
    if errors:
        raise ValueError(f"Validation errors:\n" + "\n".join(errors))
    
    # Normalize the specification
    normalized = normalize_spec(spec, config)
    
    # Parse and integrate diagram if present
    if 'diagramme' in spec:
        parsed_diagram = diagram_parser.parse_diagram(spec['diagramme'])
        normalized = diagram_parser.integrate_into_spec(normalized, parsed_diagram)
    
    # Add metadata
    normalized['metadata'] = {
        'normalized_at': datetime.utcnow().isoformat() + 'Z',
        'version': '1.0.0',
        'source_file': str(Path(input_path).resolve()),
        'hash': trace.sha256_text(json.dumps(spec, ensure_ascii=False, sort_keys=True))
    }
    
    # Write normalized specification with stable ordering
    output_file = out_path / 'normalized_spec.json'
    fs.write_json(str(output_file), normalized, sort_keys=True)
    
    # Create trace artifacts
    trace.write_sidecar_hash(output_file)
    trace.write_meta(output_file, kind='normalized_spec', extra={'source_file': str(Path(input_path).resolve())})
    
    return str(output_file)


def validate_spec(spec: Dict) -> List[str]:
    """
    Validate specification against input_spec.schema.json.
    
    Args:
        spec: Input specification dictionary
        
    Returns:
        List of validation error messages
    """
    errors = []
    
    # Check required fields
    required = ['title', 'fonctionnalites', 'exigences', 'mcd', 'nommage', 
                'dialecte_cobol', 'sql_cible']
    for field in required:
        if field not in spec:
            errors.append(f"Missing required field: {field}")
    
    # Validate dialecte_cobol
    if 'dialecte_cobol' in spec:
        if spec['dialecte_cobol'] not in ['gnucobol', 'ibm', 'microfocus']:
            errors.append(f"Invalid dialecte_cobol: {spec['dialecte_cobol']}")
    
    # Validate sql_cible
    if 'sql_cible' in spec:
        if spec['sql_cible'] not in ['postgres', 'mysql', 'oracle', 'sqlserver', 'db2', 'sqlite']:
            errors.append(f"Invalid sql_cible: {spec['sql_cible']}")
    
    # Validate fonctionnalites
    if 'fonctionnalites' in spec:
        if not isinstance(spec['fonctionnalites'], list):
            errors.append("fonctionnalites must be an array")
        else:
            for idx, func in enumerate(spec['fonctionnalites']):
                if not isinstance(func, dict):
                    errors.append(f"fonctionnalites[{idx}] must be an object")
                elif 'id' not in func or 'resume' not in func:
                    errors.append(f"fonctionnalites[{idx}] missing required fields: id, resume")
                elif 'id' in func and not re.match(r'^[A-Z][A-Z0-9_-]+$', func['id']):
                    errors.append(f"fonctionnalites[{idx}].id has invalid format: {func['id']}")
    
    # Validate exigences
    if 'exigences' in spec:
        if not isinstance(spec['exigences'], list):
            errors.append("exigences must be an array")
        else:
            for idx, req in enumerate(spec['exigences']):
                if not isinstance(req, dict):
                    errors.append(f"exigences[{idx}] must be an object")
                elif not all(k in req for k in ['id', 'type', 'regle']):
                    errors.append(f"exigences[{idx}] missing required fields")
                else:
                    ALLOWED = {'business','logic','dal','validation','calcul','transformation'} | set(SYNONYMS_EXIG_TYPE.keys()) | set(SYNONYMS_EXIG_TYPE.values())
                    if 'type' in req and req['type'] not in ALLOWED:
                        errors.append(f"exigences[{idx}].type is invalid: {req['type']}")
    
    # Validate MCD entities
    if 'mcd' in spec:
        if not isinstance(spec['mcd'], dict) or 'entites' not in spec['mcd']:
            errors.append("mcd must contain 'entites' array")
        else:
            for idx, entity in enumerate(spec['mcd']['entites']):
                if 'name' not in entity:
                    errors.append(f"mcd.entites[{idx}] missing 'name'")
                elif not re.match(r'^[A-Z][A-Z0-9_]*$', entity['name']):
                    errors.append(f"mcd.entites[{idx}].name has invalid format: {entity['name']}")
                
                if 'attrs' not in entity:
                    errors.append(f"mcd.entites[{idx}] missing 'attrs'")
                else:
                    for attr_idx, attr in enumerate(entity['attrs']):
                        if 'name' not in attr or 'type' not in attr:
                            errors.append(f"mcd.entites[{idx}].attrs[{attr_idx}] missing required fields")
    
    # Validate nommage
    if 'nommage' in spec:
        if not isinstance(spec['nommage'], dict):
            errors.append("nommage must be an object")
        else:
            if 'variables_case' not in spec['nommage'] or 'programmes_case' not in spec['nommage']:
                errors.append("nommage missing required fields: variables_case, programmes_case")
            else:
                valid_cases = ['UPPER_SNAKE', 'UPPER-KEBAB', 'lower_snake', 'camelCase', 'PascalCase']
                if spec['nommage']['variables_case'] not in valid_cases:
                    errors.append(f"Invalid variables_case: {spec['nommage']['variables_case']}")
                if spec['nommage']['programmes_case'] not in valid_cases:
                    errors.append(f"Invalid programmes_case: {spec['nommage']['programmes_case']}")
    
    return errors


def normalize_spec(spec: Dict, config: Dict) -> Dict:
    """
    Normalize specification: types, casing, defaults.
    
    Args:
        spec: Validated input specification
        config: Project configuration
        
    Returns:
        Normalized specification dictionary
    """
    normalized = json.loads(json.dumps(spec))  # Deep copy
    
    # Normalize casing settings
    naming = normalized.get('nommage', {})
    naming['copybooks_case'] = naming.get('copybooks_case', 'UPPER_SNAKE')
    naming['tables_case'] = naming.get('tables_case', 'UPPER_SNAKE')
    normalized['nommage'] = naming
    
    # Normalize fonctionnalites
    for func in normalized.get('fonctionnalites', []):
        func['normalized'] = {
            'scope': 'module',
            'components': []
        }
    
    # Normalize exigences
    for req in normalized.get('exigences', []):
        req['normalized'] = {
            'layer': map_requirement_type_to_layer(req['type']),
            'priority': 5,  # Default priority
            'affects_entities': extract_entities_from_rule(req['regle'])
        }
    
    # Normalize MCD entities and attributes
    for entity in normalized.get('mcd', {}).get('entites', []):
        entity['normalized_name'] = apply_case(entity['name'], naming.get('tables_case', 'UPPER_SNAKE'))
        
        for attr in entity.get('attrs', []):
            # Normalize attribute name
            attr['normalized_name'] = apply_case(attr['name'], naming.get('variables_case', 'UPPER_SNAKE'))
            
            # Normalize SQL type
            sql_type = attr['type']
            normalized_type = typing_map.normalize_sql_type(sql_type)
            attr['normalized_type'] = normalized_type
            
            # Set defaults
            if 'nullable' not in attr:
                attr['nullable'] = False if attr.get('pk') else True
            
            if 'auto_generated' not in attr:
                attr['auto_generated'] = detect_auto_generated(attr['name'], attr['type'])
            
            if 'default' not in attr:
                attr['default'] = None
    
    # Normalize relations
    if 'relations' in normalized.get('mcd', {}):
        for rel in normalized['mcd']['relations']:
            rel['normalized'] = {
                'cardinality': rel['type'],
                'foreign_keys': []
            }
    
    # Ensure consistent line endings (LF)
    # This is handled at the file writing level
    
    return normalized


def map_requirement_type_to_layer(req_type: str) -> str:
    """Map requirement type to architecture layer."""
    mapping = {
        'business': 'business',
        'logic': 'logic',
        'calcul': 'logic',
        'transformation': 'logic',
        'validation': 'logic',
        'dal': 'dal'
    }
    return mapping.get(req_type, 'logic')


def extract_entities_from_rule(rule: str) -> List[str]:
    """Extract entity names mentioned in a rule text."""
    entities = []
    # Simple heuristic: look for capitalized words
    words = re.findall(r'\b[A-Z][A-Z0-9_]*\b', rule)
    for word in words:
        if len(word) > 2:  # Avoid short acronyms
            entities.append(word)
    return list(set(entities))


def apply_case(name: str, case_style: str) -> str:
    """Apply casing style to a name."""
    if case_style == 'UPPER_SNAKE':
        return name.upper().replace('-', '_')
    elif case_style == 'UPPER-KEBAB':
        return name.upper().replace('_', '-')
    elif case_style == 'lower_snake':
        return name.lower().replace('-', '_')
    elif case_style == 'camelCase':
        parts = name.replace('-', '_').split('_')
        return parts[0].lower() + ''.join(p.capitalize() for p in parts[1:])
    elif case_style == 'PascalCase':
        parts = name.replace('-', '_').split('_')
        return ''.join(p.capitalize() for p in parts)
    else:
        return name


def detect_auto_generated(name: str, sql_type: str) -> bool:
    """Detect if a field is likely auto-generated."""
    name_lower = name.lower()
    type_lower = sql_type.lower()
    
    # Common auto-generated patterns
    auto_patterns = [
        'created_at', 'updated_at', 'modified_at',
        'created_date', 'updated_date', 'modified_date',
        'timestamp', 'last_modified'
    ]
    
    for pattern in auto_patterns:
        if pattern in name_lower:
            return True
    
    # Serial/identity types
    if 'serial' in type_lower or 'identity' in type_lower:
        return True
    
    return False
