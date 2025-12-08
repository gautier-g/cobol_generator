"""
Validation and normalization module for input specifications.
Reads input spec, validates against schema, normalizes types and casing.
"""

import json
import re
from datetime import datetime
from pathlib import Path

def _resolve_input_schema_path() -> Path:
    # Cherche d'abord dans le paquet, puis à la racine
    cands = [
        Path(__file__).resolve().parent.parent / 'schemas' / 'input_spec.schema.json',
        Path.cwd() / 'cobolsmartgen' / 'schemas' / 'input_spec.schema.json',
        Path.cwd() / 'schemas' / 'input_spec.schema.json',
    ]
    for c in cands:
        if c.exists():
            return c
    # Dernier recours: chemin classique (peut ne pas exister)
    return _resolve_input_schema_path()

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