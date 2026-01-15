# file: cobolsmartgen/generate/code_context_builder.py
"""
Code Context Builder - Extrait et formate le code déjà généré pour le passer aux prompts suivants.

Suit l'approche :
1. SQL + structures COBOL (étape 2)
2. DAL (étape 3) - utilise structures
3. Métier/Logic (étape 4) - utilise structures
4. Business (étape 5) - utilise DAL + Logic + structures

Chaque étape reçoit le code des étapes précédentes dans son prompt.
"""
import re
import logging
from pathlib import Path
from typing import Dict, List, Optional

LOG = logging.getLogger(__name__)


class ProgramInterface:
    """Interface d'un programme COBOL déjà généré."""

    def __init__(self, program_id: str, layer: str):
        self.program_id = program_id
        self.layer = layer
        self.paragraphs: List[Dict] = []  # {name, description, inputs, outputs}
        self.data_structures: List[str] = []  # Code des 01 structures
        self.control_variables: List[Dict] = []  # {name, pic, description}

    def add_paragraph(self, name: str, description: str = "", inputs: List[str] = None, outputs: List[str] = None):
        """Ajoute un paragraphe à l'interface."""
        self.paragraphs.append({
            "name": name,
            "description": description,
            "inputs": inputs or [],
            "outputs": outputs or []
        })

    def add_data_structure(self, structure_code: str):
        """Ajoute une structure de données (01 level)."""
        self.data_structures.append(structure_code)

    def add_control_variable(self, name: str, pic: str, description: str = ""):
        """Ajoute une variable de contrôle."""
        self.control_variables.append({
            "name": name,
            "pic": pic,
            "description": description
        })


def extract_program_interface(cobol_file: Path, layer: str) -> Optional[ProgramInterface]:
    """
    Extrait l'interface publique d'un programme COBOL.

    Args:
        cobol_file: Chemin du fichier .cbl
        layer: Couche (dal, logic, business)

    Returns:
        ProgramInterface ou None si fichier inexistant/vide
    """
    if not cobol_file.exists():
        LOG.warning(f"File not found: {cobol_file}")
        return None

    try:
        content = cobol_file.read_text()
    except Exception as e:
        LOG.error(f"Failed to read {cobol_file}: {e}")
        return None

    if not content.strip():
        LOG.warning(f"Empty file: {cobol_file}")
        return None

    # Extraire PROGRAM-ID
    program_id_match = re.search(r'PROGRAM-ID\.\s+([A-Z0-9\-]+)', content, re.IGNORECASE)
    if not program_id_match:
        LOG.warning(f"No PROGRAM-ID found in {cobol_file}")
        return None

    program_id = program_id_match.group(1)
    interface = ProgramInterface(program_id, layer)

    # Extraire les paragraphes (lignes qui se terminent par '.') avec indentation tolérée
    # Format attendu: PARAGRAPH-NAME.
    paragraph_pattern = re.compile(r'^\s*([A-Z0-9\-]+)\.\s*$', re.MULTILINE)
    for match in paragraph_pattern.finditer(content):
        para_name = match.group(1)
        # Ignorer les sections/divisions
        if para_name.upper() in ['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE',
                                   'WORKING-STORAGE', 'LINKAGE', 'FILE', 'LOCAL-STORAGE']:
            continue
        # Ajouter le paragraphe
        interface.add_paragraph(para_name, description=_infer_paragraph_description(para_name, layer))

    # Extraire les structures de données (01 level)
    structure_pattern = re.compile(r'(^\s*01\s+[A-Z0-9\-]+.*?(?=^\s*01\s|\Z))', re.MULTILINE | re.DOTALL)
    for match in structure_pattern.finditer(content):
        structure = match.group(1).strip()
        # Ne garder que les structures métier (pas SQLCA, etc.)
        if not any(skip in structure.upper() for skip in ['SQLCA', 'SQLDA', 'WS-TEMP', 'WS-WORK']):
            interface.add_data_structure(structure)

    # Extraire les variables de contrôle importantes
    control_vars = [
        (r'(END-OF-FILE|EOF)\s+PIC\s+([X9A]+(?:\([0-9]+\))?)', "Indicateur de fin de fichier/curseur"),
        (r'(WS-RETURN-CODE|RETURN-CODE)\s+PIC\s+([X9A]+(?:\([0-9]+\))?)', "Code retour de la dernière opération"),
        (r'(WS-EOF-FLAG)\s+PIC\s+([X9A]+(?:\([0-9]+\))?)', "Flag de fin de fichier"),
        (r'(SQLCODE)\s+PIC\s+([S9]+(?:\([0-9]+\))?)', "Code retour SQL"),
    ]

    for pattern, desc in control_vars:
        match = re.search(pattern, content, re.IGNORECASE)
        if match:
            interface.add_control_variable(match.group(1), match.group(2), desc)

    LOG.info(f"Extracted interface from {program_id}: {len(interface.paragraphs)} paragraphs, "
             f"{len(interface.data_structures)} structures, {len(interface.control_variables)} control vars")

    return interface


def _infer_paragraph_description(para_name: str, layer: str) -> str:
    """Infère une description à partir du nom du paragraphe et de la couche."""
    name_upper = para_name.upper()

    # DAL descriptions
    if layer == "dal":
        if "OPEN" in name_upper and "CURSOR" in name_upper:
            return "Ouvre le curseur sur la table"
        elif "CLOSE" in name_upper and "CURSOR" in name_upper:
            return "Ferme le curseur"
        elif "FETCH" in name_upper:
            return "Lit l'enregistrement suivant du curseur"
        elif "READ" in name_upper or "SELECT" in name_upper:
            return "Lit un enregistrement depuis la base"
        elif "INSERT" in name_upper or "CREATE" in name_upper:
            return "Insère un nouvel enregistrement"
        elif "UPDATE" in name_upper:
            return "Met à jour un enregistrement existant"
        elif "DELETE" in name_upper:
            return "Supprime un enregistrement"
        elif "OPEN" in name_upper and "DATABASE" in name_upper:
            return "Ouvre la connexion à la base de données"
        elif "CLOSE" in name_upper and "DATABASE" in name_upper:
            return "Ferme la connexion à la base de données"

    # LOGIC descriptions
    elif layer == "logic":
        if "CALCULATE" in name_upper or "COMPUTE" in name_upper:
            return "Effectue un calcul métier"
        elif "VALIDATE" in name_upper:
            return "Valide les données métier"
        elif "PROCESS" in name_upper:
            return "Traite une logique métier"
        elif "CHECK" in name_upper:
            return "Vérifie une condition métier"

    # BUSINESS descriptions
    elif layer == "business":
        if "MENU" in name_upper:
            return "Affiche un menu utilisateur"
        elif "DISPLAY" in name_upper or "SHOW" in name_upper:
            return "Affiche des informations"
        elif "MAIN" in name_upper:
            return "Point d'entrée principal"
        elif "ORCHESTRATE" in name_upper or "COORDINATE" in name_upper:
            return "Orchestre les appels aux couches inférieures"

    return "Procédure"


def format_interface_for_prompt(interface: ProgramInterface) -> str:
    """
    Formate l'interface d'un programme pour l'inclure dans un prompt.

    Returns:
        String formaté prêt à injecter dans le prompt
    """
    lines = []

    lines.append(f"=== PROGRAMME {interface.layer.upper()} DÉJÀ GÉNÉRÉ : {interface.program_id} ===")
    lines.append("")
    lines.append(f"  CE PROGRAMME EXISTE DÉJÀ - TU DOIS LE RÉUTILISER TEL QUEL")
    lines.append("")

    # Paragraphes disponibles
    if interface.paragraphs:
        lines.append(f"PARAGRAPHES DISPONIBLES ({len(interface.paragraphs)}) :")
        for para in interface.paragraphs:
            lines.append(f"  • {para['name']}")
            if para['description']:
                lines.append(f"      → {para['description']}")
            if para['inputs']:
                lines.append(f"      Input: {', '.join(para['inputs'])}")
            if para['outputs']:
                lines.append(f"      Output: {', '.join(para['outputs'])}")
        lines.append("")

    # Variables de contrôle
    if interface.control_variables:
        lines.append("VARIABLES DE CONTRÔLE :")
        for var in interface.control_variables:
            desc = f" - {var['description']}" if var['description'] else ""
            lines.append(f"  • {var['name']} PIC {var['pic']}{desc}")
        lines.append("")

    # Structures de données
    if interface.data_structures:
        lines.append("STRUCTURES DE DONNÉES (COPYBOOK) :")
        for struct in interface.data_structures[:3]:  # Limiter à 3 pour ne pas surcharger
            lines.append("")
            for line in struct.split('\n'):
                lines.append(f"  {line}")
        if len(interface.data_structures) > 3:
            lines.append(f"  ... et {len(interface.data_structures) - 3} autres structures")
        lines.append("")

    lines.append("  RÈGLES D'UTILISATION :")
    lines.append("  • PERFORM ces paragraphes pour utiliser ce programme")
    lines.append("  • Réutilise EXACTEMENT les noms de variables des structures")
    lines.append("  • Ne modifie PAS ce code, seulement appelle-le")

    return "\n".join(lines)


def build_code_context(output_dir: Path, current_layer: str, current_program: str) -> str:
    """
    Construit le contexte de code pour un programme en cours de génération.

    Inclut le code déjà généré des couches précédentes.

    Args:
        output_dir: Répertoire de sortie (contient dal/, logic/, business/)
        current_layer: Couche du programme en cours de génération (dal/logic/business)
        current_program: ID du programme en cours de génération

    Returns:
        Contexte formaté à inclure dans le prompt
    """
    context_parts = []

    # Ordre de génération : dal -> logic -> business
    # On inclut toutes les couches précédentes

    layers_order = ["dal", "logic", "business"]
    current_index = layers_order.index(current_layer)

    # Parcourir les couches précédentes
    for i in range(current_index):
        layer = layers_order[i]
        layer_dir = output_dir / layer

        if not layer_dir.exists():
            continue

        # Trouver tous les fichiers .cbl dans cette couche
        for cbl_file in sorted(layer_dir.glob("*.cbl")):
            interface = extract_program_interface(cbl_file, layer)
            if interface:
                context_parts.append(format_interface_for_prompt(interface))
                context_parts.append("")  # Ligne vide entre programmes

    if not context_parts:
        return ""

    # Assembler le contexte final
    header = [
        "=" * 80,
        "CODE DÉJÀ GÉNÉRÉ (COUCHES PRÉCÉDENTES)",
        "=" * 80,
        "",
        "Les programmes suivants ont déjà été générés.",
        "Tu DOIS les réutiliser en appelant leurs paragraphes.",
        "Tu NE DOIS PAS modifier leur code, seulement l'utiliser.",
        "",
    ]

    return "\n".join(header + context_parts)
