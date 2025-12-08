# file: cobolsmartgen/core/contract_generator.py
"""
Contract Generator - Génère le contrat d'architecture immuable.

Ce contrat définit :
- Les variables autorisées par programme
- Les procédures autorisées par programme
- Les responsabilités par couche
- Les patterns interdits

Une fois généré, le contrat devient READ-ONLY et gouverne tous les agents.
"""
from __future__ import annotations
import json
import logging
import os
from pathlib import Path
from typing import Dict, List, Set

from ..utils import fs

LOG = logging.getLogger(__name__)

# Variables globales autorisées pour tous les programmes
GLOBAL_ALLOWED_VARIABLES = {
    "END-OF-FILE",
    "EOF",
    "SQLCA",
    "SQLCODE",
    "SQLERRM",
    "SQLERRML",
    "SQLERRMC",
    "SQLERRP",
    "SQLSTATE",
    "SQLWARN",
    "SQLERRD",
    "SQLERRD-1",
    "SQLERRD-2",
    "SQLERRD-3",
    "WS-RETURN-CODE",
    "RETURN-CODE",
    "STATUS-CODE",
    "WS-EOF-FLAG",
}

# Patterns COBOL interdits (sauf si explicitement requis)
FORBIDDEN_PATTERNS = [
    "LINKAGE SECTION (unless explicitly needed for CALL parameters)",
    "FILE SECTION (unless file I/O explicitly required)",
    "REPORT SECTION",
    "ENVIRONMENT DIVISION (unless file I/O explicitly required)",
    "INDEXED files (use LINE SEQUENTIAL instead)",
]

# Responsabilités par couche (architecture 3-tier)
LAYER_RESPONSIBILITIES = {
    "dal": {
        "allowed": [
            "database_access",
            "sql_operations",
            "cursor_management",
            "connection_management",
        ],
        "forbidden": [
            "business_logic",
            "calculations",
            "user_interface",
            "orchestration",
        ],
    },
    "logic": {
        "allowed": [
            "business_rules",
            "calculations",
            "validations",
            "data_transformations",
        ],
        "forbidden": [
            "database_access",
            "sql_operations",
            "user_interface",
            "menu_display",
        ],
    },
    "business": {
        "allowed": [
            "orchestration",
            "user_interface",
            "menu_display",
            "process_coordination",
        ],
        "forbidden": [
            "database_access",
            "sql_operations",
            "business_calculations",
        ],
    },
}


def _extract_variables_from_io_map(io_map: Dict, entity_name: str) -> Set[str]:
    """Extrait les variables autorisées depuis io_map pour une entité."""
    variables = set()

    for entity in io_map.get("entities", []):
        if entity.get("name") == entity_name or entity.get("entity_name") == entity_name:
            # Inputs
            for inp in entity.get("inputs", []) or []:
                var_name = inp.get("cobol_name")
                if var_name:
                    variables.add(var_name.upper())

            # Outputs
            for out in entity.get("outputs", []) or []:
                var_name = out.get("cobol_name")
                if var_name:
                    variables.add(var_name.upper())

            # Fields
            for field in entity.get("fields", []) or []:
                var_name = field.get("cobol_name")
                if var_name:
                    variables.add(var_name.upper())

    return variables


def _extract_procedures_from_plan(program_plan: Dict, program_id: str) -> List[str]:
    """Extrait les procédures autorisées depuis program_plan pour un programme."""
    procedures = []

    for program in program_plan.get("programs", []):
        pid = program.get("id") or program.get("name")
        if pid == program_id:
            for proc in program.get("procedures", []) or []:
                proc_name = proc.get("name")
                if proc_name:
                    procedures.append(proc_name.upper())

    return procedures


def _infer_layer_from_program_id(program_id: str) -> str:
    """Infère la couche depuis le nom du programme."""
    pid_upper = program_id.upper()

    if "DAL" in pid_upper or "DATA-ACCESS" in pid_upper:
        return "dal"
    elif "LOGIC" in pid_upper or "APP" in pid_upper:
        return "logic"
    elif "BUSINESS" in pid_upper or "UI" in pid_upper or "MENU" in pid_upper:
        return "business"
    else:
        # Défaut : logic
        return "logic"


def generate_contract(
    out_dir: str,
    normalized_spec: Dict,
    io_map: Dict,
    program_plan: Dict,
) -> Dict:
    """
    Génère le contrat d'architecture à partir des specs.

    Args:
        out_dir: Répertoire de sortie
        normalized_spec: Spécifications normalisées
        io_map: Mapping IO
        program_plan: Plan des programmes

    Returns:
        Le contrat d'architecture (dict)
    """
    LOG.info("🔒 Generating architecture contract...")

    contract = {
        "version": "1.0",
        "project": normalized_spec.get("projet", {}).get("nom", "UNKNOWN"),
        "layers": {},
        "allowed_variables": {
            "global": sorted(list(GLOBAL_ALLOWED_VARIABLES))
        },
        "allowed_procedures": {},
        "forbidden_patterns": FORBIDDEN_PATTERNS,
        "compilation_rules": {
            "dialect": normalized_spec.get("dialecte_cobol", "gnucobol"),
            "format": "free" if normalized_spec.get("dialecte_cobol") == "gnucobol" else "fixed",
            "max_compilation_attempts": int(os.getenv("CSG_COMPILE_MAX_PASSES", "3")),
            "max_llm_corrections_per_file": int(os.getenv("CSG_MAX_LLM_CORRECTIONS", "1")),
            "fail_fast": os.getenv("CSG_FAIL_FAST", "1") == "1",
        },
    }

    # Parcourir tous les programmes du plan
    programs_by_layer = {"dal": [], "logic": [], "business": []}

    for program in program_plan.get("programs", []):
        program_id = program.get("id") or program.get("name", "PROGRAM")
        layer = program.get("layer") or _infer_layer_from_program_id(program_id)
        entity_name = program.get("entity", "")

        # Ajouter à la couche
        if layer not in programs_by_layer:
            programs_by_layer[layer] = []
        programs_by_layer[layer].append(program_id)

        # Extraire les variables autorisées pour ce programme
        entity_variables = _extract_variables_from_io_map(io_map, entity_name)

        # Ajouter des variables standard par couche
        layer_specific_variables = set()
        if layer == "dal":
            layer_specific_variables = {"WS-EOF-FLAG", "WS-RETURN-CODE"}
        elif layer == "logic":
            layer_specific_variables = set()
        elif layer == "business":
            layer_specific_variables = {"WS-USER-OPTION", "WS-EMPLOYEE-COUNT", "WS-CONFIRM-FLAG"}

        # Combiner
        all_variables = entity_variables | layer_specific_variables

        contract["allowed_variables"][program_id] = sorted(list(all_variables))

        # Extraire les procédures autorisées
        procedures = _extract_procedures_from_plan(program_plan, program_id)
        contract["allowed_procedures"][program_id] = procedures

    # Définir les couches
    for layer_name, layer_config in LAYER_RESPONSIBILITIES.items():
        contract["layers"][layer_name] = {
            "programs": programs_by_layer.get(layer_name, []),
            "responsibilities": layer_config["allowed"],
            "forbidden": layer_config["forbidden"],
        }

    # Sauvegarder le contrat
    contract_path = Path(out_dir) / "architecture_contract.json"
    fs.write_json(str(contract_path), contract, sort_keys=True, atomic=True)

    # Rendre le fichier read-only (permissions 444)
    try:
        os.chmod(contract_path, 0o444)
        LOG.info(f"   🔒 Contract locked (read-only): {contract_path}")
    except Exception as e:
        LOG.warning(f"   ⚠️  Could not lock contract file: {e}")

    LOG.info(f"   ✅ Contract generated: {contract_path}")
    LOG.info(f"   📋 Programs: {sum(len(v) for v in programs_by_layer.values())}")
    LOG.info(f"   📋 Layers: {list(contract['layers'].keys())}")

    return contract


def load_contract(out_dir: str) -> Dict:
    """Charge le contrat d'architecture."""
    contract_path = Path(out_dir) / "architecture_contract.json"

    if not contract_path.exists():
        raise FileNotFoundError(f"Contract not found: {contract_path}")

    return fs.read_json(str(contract_path))


def validate_against_contract(
    cobol_code: str,
    contract: Dict,
    program_id: str,
) -> List[Dict]:
    """
    Valide que le code COBOL respecte le contrat.

    Returns:
        Liste des violations (vide si conforme)
    """
    import re

    violations = []

    # Extraire les variables déclarées dans le code
    # Pattern pour détecter les déclarations de variables (01, 05, 77, 78, 88)
    var_pattern = re.compile(
        r'^\s*(?:0?[1-9][0-9]|77|78|88)\s+([A-Z0-9\-]+)',
        re.MULTILINE | re.IGNORECASE
    )

    declared_vars = set()
    for match in var_pattern.finditer(cobol_code):
        var_name = match.group(1).upper()
        declared_vars.add(var_name)

    # Variables autorisées
    allowed_vars = set(contract["allowed_variables"]["global"])
    if program_id in contract["allowed_variables"]:
        allowed_vars |= set(contract["allowed_variables"][program_id])

    # Violations de variables
    unauthorized_vars = declared_vars - allowed_vars
    if unauthorized_vars:
        violations.append({
            "type": "unauthorized_variable",
            "severity": "ERROR",
            "items": sorted(list(unauthorized_vars)),
            "message": f"Variables not in contract: {', '.join(sorted(unauthorized_vars))}",
        })

    # Extraire les procédures/paragraphes
    proc_pattern = re.compile(
        r'^\s+([A-Z0-9\-]+)\.\s*$',
        re.MULTILINE
    )

    declared_procs = set()
    for match in proc_pattern.finditer(cobol_code):
        proc_name = match.group(1).upper()
        # Filtrer les mots-clés COBOL (END-IF, END-PERFORM, etc.)
        if not proc_name.startswith("END-"):
            declared_procs.add(proc_name)

    # Procédures autorisées
    allowed_procs = set()
    if program_id in contract["allowed_procedures"]:
        allowed_procs = set(contract["allowed_procedures"][program_id])

    # Violations de procédures (WARNING seulement, car peut être légitime)
    unauthorized_procs = declared_procs - allowed_procs
    if unauthorized_procs:
        violations.append({
            "type": "unauthorized_procedure",
            "severity": "WARNING",
            "items": sorted(list(unauthorized_procs)),
            "message": f"Procedures not in plan: {', '.join(sorted(unauthorized_procs))}",
        })

    # Patterns interdits
    for pattern in contract.get("forbidden_patterns", []):
        pattern_key = pattern.split("(")[0].strip().upper()

        if pattern_key in ["LINKAGE SECTION", "FILE SECTION", "REPORT SECTION"]:
            if re.search(rf'\b{pattern_key}\b', cobol_code, re.IGNORECASE):
                violations.append({
                    "type": "forbidden_pattern",
                    "severity": "WARNING",
                    "pattern": pattern,
                    "message": f"Forbidden pattern detected: {pattern}",
                })

    return violations


def run(out_dir: str, config: Dict) -> Dict:
    """Point d'entrée pour générer le contrat."""
    out = Path(out_dir)

    # Charger les fichiers nécessaires
    normalized_spec = fs.read_json(str(out / "normalized_spec.json"))
    io_map = fs.read_json(str(out / "io_map.json"))
    program_plan = fs.read_json(str(out / "program_plan.json"))

    # Générer le contrat
    contract = generate_contract(
        out_dir=out_dir,
        normalized_spec=normalized_spec,
        io_map=io_map,
        program_plan=program_plan,
    )

    return contract
