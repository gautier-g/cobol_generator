# file: cobolsmartgen/generate/prompt_formatters.py
"""
Fonctions de formatage des données JSON pour les prompts LLM.
Objectif: Extraire et formater les infos de manière propre, concise et structurée.
"""
from typing import Dict, List
import logging

LOG = logging.getLogger(__name__)


def _explain_pic_clause(pic: str) -> str:
    """
    Convertit une clause PIC COBOL en description lisible.

    Args:
        pic: Clause PIC (ex: "X(10)", "9(5)V99", "S9(7)")

    Returns:
        Description en français

    Exemples:
        X(10) -> "Texte de 10 caractères"
        9(5) -> "Nombre entier de 5 chiffres"
        S9(7) -> "Nombre entier signé de 7 chiffres"
        9(5)V99 -> "Nombre décimal (5 entiers, 2 décimales)"
    """
    import re

    if not pic:
        return "Type non spécifié"

    pic = pic.strip().upper()

    # X(n) - Alphanumérique
    match = re.match(r'^X\((\d+)\)$', pic)
    if match:
        n = match.group(1)
        return f"Texte de {n} caractères"

    # X sans parenthèses (1 caractère)
    if pic == 'X':
        return "Texte de 1 caractère"

    # 9(n)V9(m) - Décimal
    match = re.match(r'^S?9\((\d+)\)V9+\((\d+)\)$', pic)
    if match:
        n = match.group(1)
        m = match.group(2)
        signed = "signé " if pic.startswith('S') else ""
        return f"Nombre décimal {signed}({n} entiers, {m} décimales)"

    # 9(n)V99 - Décimal avec nombre fixe de décimales
    match = re.match(r'^S?9\((\d+)\)V(9+)$', pic)
    if match:
        n = match.group(1)
        m = len(match.group(2))
        signed = "signé " if pic.startswith('S') else ""
        return f"Nombre décimal {signed}({n} entiers, {m} décimales)"

    # S9(n) - Entier signé
    match = re.match(r'^S9\((\d+)\)$', pic)
    if match:
        n = match.group(1)
        return f"Nombre entier signé de {n} chiffres"

    # 9(n) - Entier
    match = re.match(r'^9\((\d+)\)$', pic)
    if match:
        n = match.group(1)
        return f"Nombre entier de {n} chiffres"

    # 9 sans parenthèses
    if pic == '9':
        return "Nombre entier de 1 chiffre"

    # S9 sans parenthèses
    if pic == 'S9':
        return "Nombre entier signé de 1 chiffre"

    # A(n) - Alphabétique
    match = re.match(r'^A\((\d+)\)$', pic)
    if match:
        n = match.group(1)
        return f"Lettres uniquement ({n} caractères)"

    # Fallback: retourner tel quel si non reconnu
    return f"PIC {pic}"


def format_io_data(io_json: Dict, entity_name: str = None) -> str:
    """
    Formate les données IO de manière concise pour le prompt.

    Args:
        io_json: Dictionnaire IO map (peut être entity ou global)
        entity_name: Nom de l'entité (optionnel)

    Returns:
        String formaté pour le prompt
    """
    if not io_json:
        return "Aucune donnée IO spécifiée."

    # Si c'est une entity (dict avec inputs/outputs/fields)
    if isinstance(io_json, dict) and any(k in io_json for k in ["inputs", "outputs", "fields"]):
        return _format_entity_io(io_json, entity_name)

    # Si c'est un string JSON (déjà formaté)
    if isinstance(io_json, str):
        try:
            import json
            io_json = json.loads(io_json)
        except:
            return io_json

    return _format_entity_io(io_json, entity_name)


def _format_entity_io(entity: Dict, entity_name: str = None) -> str:
    """Formate une entité IO."""
    lines = []

    if entity_name:
        lines.append(f"Entité: {entity_name}")

    # Inputs
    inputs = entity.get("inputs", [])
    if inputs:
        lines.append("\nVARIABLES D'ENTRÉE:")
        for inp in inputs:
            cobol_name = inp.get("cobol_name", "UNKNOWN")
            pic = inp.get("cobol_type") or inp.get("pic", "X")
            desc = inp.get("description", "")
            pic_desc = _explain_pic_clause(pic.replace("PIC ", "") if isinstance(pic, str) else pic)
            lines.append(f"  - {cobol_name}: {pic_desc}{' - ' + desc if desc else ''}")

    # Outputs
    outputs = entity.get("outputs", [])
    if outputs:
        lines.append("\nVARIABLES DE SORTIE:")
        for out in outputs:
            cobol_name = out.get("cobol_name", "UNKNOWN")
            pic = out.get("cobol_type") or out.get("pic", "X")
            desc = out.get("description", "")
            pic_desc = _explain_pic_clause(pic.replace("PIC ", "") if isinstance(pic, str) else pic)
            lines.append(f"  - {cobol_name}: {pic_desc}{' - ' + desc if desc else ''}")

    # Fields (si pas d'inputs/outputs)
    fields = entity.get("fields", [])
    if fields and not inputs and not outputs:
        lines.append("\nCHAMPS:")
        for field in fields:
            cobol_name = field.get("cobol_name", "UNKNOWN")
            pic = field.get("cobol_type") or field.get("pic", "X")
            desc = field.get("description", "")
            pic_desc = _explain_pic_clause(pic.replace("PIC ", "") if isinstance(pic, str) else pic)
            lines.append(f"  - {cobol_name}: {pic_desc}{' - ' + desc if desc else ''}")

    return "\n".join(lines) if lines else "Aucune donnée IO."


def format_requirements(exigences: List[Dict]) -> str:
    """
    Formate les exigences de manière concise par type.

    Args:
        exigences: Liste des exigences

    Returns:
        String formaté par type d'exigence
    """
    if not exigences:
        return "Aucune exigence spécifiée."

    # Regrouper par type
    by_type = {}
    for req in exigences:
        req_type = req.get("type", "general")
        by_type.setdefault(req_type, []).append(req)

    lines = []

    # Ordre de priorité
    type_order = ["business", "regle_metier", "functional", "data", "technical", "general"]

    for req_type in type_order:
        if req_type not in by_type:
            continue

        reqs = by_type[req_type]
        type_label = _get_type_label(req_type)

        lines.append(f"\n{type_label} ({len(reqs)}):")
        for req in reqs:
            req_id = req.get("id", "?")
            regle = req.get("regle", req.get("description", ""))
            priority = req.get("priority", "")

            priority_marker = "🔴" if priority == "high" else "🟡" if priority == "medium" else ""
            lines.append(f"  [{req_id}] {priority_marker} {regle}")

    # Ajouter les types non listés
    for req_type, reqs in by_type.items():
        if req_type not in type_order:
            lines.append(f"\n{req_type.upper()} ({len(reqs)}):")
            for req in reqs:
                req_id = req.get("id", "?")
                regle = req.get("regle", req.get("description", ""))
                lines.append(f"  [{req_id}] {regle}")

    return "\n".join(lines)


def _get_type_label(req_type: str) -> str:
    """Retourne un label lisible pour un type d'exigence."""
    labels = {
        "business": "RÈGLES MÉTIER",
        "regle_metier": "RÈGLES MÉTIER",
        "functional": "EXIGENCES FONCTIONNELLES",
        "data": "EXIGENCES DE DONNÉES",
        "technical": "EXIGENCES TECHNIQUES",
        "general": "EXIGENCES GÉNÉRALES",
    }
    return labels.get(req_type, req_type.upper())


def format_contract_constraints(contract: Dict, program_id: str) -> str:
    """
    Formate les contraintes du contrat pour un programme.

    Args:
        contract: Dictionnaire du contrat
        program_id: ID du programme

    Returns:
        String formaté des contraintes
    """
    if not contract:
        return ""

    lines = ["\n=== CONTRAINTES DU CONTRAT (OBLIGATOIRES) ==="]

    # Variables autorisées
    allowed_vars = contract.get("allowed_variables", {}).get(program_id, [])
    if allowed_vars:
        lines.append(f"\nVARIABLES AUTORISÉES ({len(allowed_vars)}):")
        # Limiter à 15 pour ne pas surcharger le prompt
        for var in allowed_vars[:15]:
            lines.append(f"  ✓ {var}")
        if len(allowed_vars) > 15:
            lines.append(f"  ... et {len(allowed_vars) - 15} autres (voir io_json)")
    else:
        lines.append("\nVARIABLES AUTORISÉES: Voir io_json uniquement")

    # Procédures autorisées
    allowed_procs = contract.get("allowed_procedures", {}).get(program_id, [])
    if allowed_procs:
        lines.append(f"\nPROCÉDURES AUTORISÉES ({len(allowed_procs)}):")
        for proc in allowed_procs[:10]:
            lines.append(f"  ✓ {proc}")
        if len(allowed_procs) > 10:
            lines.append(f"  ... et {len(allowed_procs) - 10} autres")

    # Patterns interdits
    forbidden = contract.get("forbidden_patterns", [])
    if forbidden:
        lines.append(f"\nPATTERNS INTERDITS ({len(forbidden)}):")
        for pattern in forbidden:
            lines.append(f"  ✗ {pattern}")

    # Règles
    lines.append("\n⚠️  RÈGLES STRICTES:")
    lines.append("  • N'ajouter AUCUNE variable non listée")
    lines.append("  • N'ajouter AUCUNE procédure non demandée")
    lines.append("  • Variables WS-* et DB-* autorisées (Working Storage)")

    return "\n".join(lines)


def format_layer_instructions(layer: str) -> str:
    """
    Retourne les instructions spécifiques pour une couche.

    Args:
        layer: dal, logic, ou business

    Returns:
        Instructions formatées
    """
    instructions = {
        "dal": """
=== RESPONSABILITÉS DAL (Data Access Layer) ===

OBJECTIF: Accès aux données uniquement

FAIRE:
  ✓ Implémenter les opérations CRUD sur l'entité
  ✓ Gérer les connexions/déconnexions base de données
  ✓ Gérer les curseurs SQL (OPEN, FETCH, CLOSE)
  ✓ Retourner les codes SQL (SQLCODE, SQLSTATE)

NE PAS FAIRE:
  ✗ Calculs métier (déléguer à LOGIC)
  ✗ Affichage (déléguer à BUSINESS)
  ✗ Validations métier (déléguer à LOGIC)

VARIABLES STANDARDS:
  • WS-RETURN-CODE : Code retour SQL
  • WS-EOF-FLAG : Fin de fichier/curseur
  • DB-* : Variables pour DECLARE SECTION
""",
        "logic": """
=== RESPONSABILITÉS LOGIC (Application Layer) ===

OBJECTIF: Logique métier et calculs

FAIRE:
  ✓ Implémenter les règles métier
  ✓ Effectuer les calculs (COMPUTE, formules)
  ✓ Valider les données métier
  ✓ Appeler DAL pour accès données

NE PAS FAIRE:
  ✗ Accès direct à la base (déléguer à DAL)
  ✗ Affichage (déléguer à BUSINESS)
  ✗ Gestion curseurs SQL (déléguer à DAL)

VARIABLES STANDARDS:
  • WS-TEMP-RESULT : Résultats intermédiaires
  • WS-VALIDATION-FLAG : Flags de validation
  • WS-CONSTANTS : Constantes métier
""",
        "business": """
=== RESPONSABILITÉS BUSINESS (Presentation Layer) ===

OBJECTIF: Orchestration et présentation

FAIRE:
  ✓ Orchestrer les appels LOGIC/DAL
  ✓ Afficher les résultats (DISPLAY)
  ✓ Gérer le flux d'exécution principal
  ✓ Présenter les données à l'utilisateur

NE PAS FAIRE:
  ✗ Calculs métier (déléguer à LOGIC)
  ✗ Accès direct base (déléguer à DAL)
  ✗ Ajouter menus non demandés

VARIABLES STANDARDS:
  • WS-COUNTER : Compteurs d'affichage
  • WS-USER-OPTION : Options utilisateur
  • WS-STATUS : Statuts d'exécution
"""
    }

    return instructions.get(layer, "")


def format_rag_rules(normalized_spec: Dict) -> str:
    """
    Extrait et formate les règles métier depuis les exigences.

    Args:
        normalized_spec: Spec normalisée

    Returns:
        Règles formatées
    """
    exigences = normalized_spec.get("exigences", [])

    rules = []
    for req in exigences:
        if req.get("type") in ["business", "regle_metier"]:
            rule_text = req.get("regle", req.get("description", ""))
            if rule_text:
                req_id = req.get("id", "?")
                rules.append(f"[{req_id}] {rule_text}")

    if not rules:
        return "Aucune règle métier spécifique."

    return "\n".join([f"  • {rule}" for rule in rules])


def build_concise_prompt(
    program_id: str,
    layer: str,
    io_data: Dict,
    exigences: List[Dict],
    contract: Dict,
    normalized_spec: Dict,
    dialecte: str = "gnucobol",
    code_context: str = "",
    pipeline_notes: str = "",
    copybook_context: str = "",
    sql_context: str = ""
) -> str:
    """
    Construit un prompt concis et structuré pour la génération COBOL.

    Args:
        code_context: Contexte du code déjà généré (couches précédentes)

    Returns:
        Prompt optimisé
    """
    entity_name = (
        normalized_spec.get("entite", {}).get("nom", "")
        or (io_data.get("entity_name") if isinstance(io_data, dict) else "")
        or (io_data.get("name") if isinstance(io_data, dict) else "")
    )

    # Construire le prompt avec ou sans contexte de code
    parts = [f"Génère un programme COBOL pour {program_id} (couche {layer.upper()})"]

    # Rappels pipeline / dépendances
    if pipeline_notes:
        parts.append("\n=== PIPELINE / CONTRAINTES ===")
        parts.append(pipeline_notes.strip())
        parts.append("")

    # SQL d'abord, puis structure COPIE/EMPLOYEE
    if sql_context:
        parts.append("=== SQL (DDL) RÉFÉRENCE ===")
        parts.append(sql_context.strip())
        parts.append("")

    if copybook_context:
        parts.append("=== STRUCTURE COBOL (COPYBOOK) RÉFÉRENCE ===")
        parts.append(copybook_context.strip())
        parts.append("")

    # Si on a du code déjà généré, le montrer EN PREMIER après les références
    if code_context:
        parts.append("=== CODE DÉJÀ GÉNÉRÉ (À RÉUTILISER, NE PAS MODIFIER) ===")
        parts.append(code_context)
        parts.append("")

    parts.append(f"""
=== CONTEXTE ===
Programme: {program_id}
Entité: {entity_name}
Dialecte: {dialecte}
Format: free (GnuCOBOL)

{format_layer_instructions(layer)}

=== DONNÉES (IO_MAP) ===
{format_io_data(io_data, entity_name)}

=== EXIGENCES ===
{format_requirements(exigences)}

=== RÈGLES MÉTIER ===
{"N/A (pas de logique métier dans la couche DAL)" if layer.lower()=="dal" else format_rag_rules(normalized_spec)}

{format_contract_constraints(contract, program_id)}

=== INSTRUCTIONS GÉNÉRATION ===
1. Génère UNIQUEMENT le code COBOL (pas de markdown, pas d'explications)
2. Utilise UNIQUEMENT les variables listées dans IO_MAP
3. Respecte les responsabilités de la couche {layer.upper()}
4. Code minimal : n'ajoute rien de non demandé
5. Variables WS-* et DB-* autorisées pour Working Storage""")

    # Si on a du contexte de code, ajouter des instructions spécifiques
    if code_context:
        parts.append("6. RÉUTILISE les programmes déjà générés (voir ci-dessus)")
        parts.append("7. APPELLE leurs paragraphes avec PERFORM")
        parts.append("8. N'INVENTE PAS de nouveaux paragraphes si des équivalents existent déjà")

    parts.append("\nGénère maintenant le programme COBOL complet:")

    return "\n".join(parts)
