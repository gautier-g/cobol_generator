# file: cobolsmartgen/generate/cobol_constraints.py
"""
COBOL constraints hardcoded in Python (not in YAML specs).

These constraints are universal to GnuCOBOL and should be added to prompts
regardless of the spec content.

IMPORTANT: These constraints are added by the pipeline, not stored in specs.
"""

def get_gnucobol_intrinsic_functions_guide() -> str:
    """
    Get comprehensive guide on GnuCOBOL intrinsic functions.

    This guide prevents LLM from using non-existent functions like CEILING, CEIL, etc.
    Added to prompts by pipeline (not in specs).

    Returns:
        Formatted text for prompt inclusion
    """
    return """=== FONCTIONS INTRINSÈQUES GNUCOBOL (CRITIQUE) ===
GnuCOBOL supporte un ensemble LIMITÉ de fonctions intrinsèques.

FONCTIONS DISPONIBLES (utilise UNIQUEMENT ces fonctions):
- FUNCTION INTEGER(x)      : Partie entière (tronque vers zéro)
- FUNCTION ANINT(x)        : Arrondi au plus proche
- FUNCTION MOD(x, y)       : Modulo
- FUNCTION MAX(x, y, ...)  : Maximum
- FUNCTION MIN(x, y, ...)  : Minimum
- FUNCTION ABS(x)          : Valeur absolue
- FUNCTION LENGTH(x)       : Longueur chaîne
- FUNCTION UPPER-CASE(x)   : Majuscules
- FUNCTION LOWER-CASE(x)   : Minuscules
- FUNCTION TRIM(x)         : Supprimer espaces
- FUNCTION CURRENT-DATE    : Date/heure système
- FUNCTION WHEN-COMPILED   : Date compilation
- FUNCTION NUMVAL(x)       : Convertir string en numérique

FONCTIONS NON DISPONIBLES (N'EXISTENT PAS):
❌ FUNCTION CEILING(x)  : N'existe pas! Utilise INTEGER(x + 0.999) à la place
❌ FUNCTION CEIL(x)     : N'existe pas! Utilise INTEGER(x + 0.999) à la place
❌ FUNCTION FLOOR(x)    : N'existe pas! Utilise INTEGER(x) à la place
❌ FUNCTION ROUND(x, n) : N'existe pas! Utilise COMPUTE ... ROUNDED à la place
❌ FUNCTION SQRT(x)     : N'existe pas!
❌ FUNCTION POW(x, y)   : N'existe pas!
❌ FUNCTION EXP(x)      : N'existe pas!
❌ FUNCTION LOG(x)      : N'existe pas!
❌ IF var NUMERIC = FALSE : N'existe pas! Utilise IF var IS NOT NUMERIC à la place

ARRONDI SUPÉRIEUR (équivalent CEILING):
Pour arrondir au supérieur, utilise cette technique:
  COMPUTE result = FUNCTION INTEGER(value + 0.999)

EXEMPLE CEILING:
  value = 5.2  → FUNCTION INTEGER(5.2 + 0.999) = INTEGER(6.199) = 6  ✓
  value = 5.0  → FUNCTION INTEGER(5.0 + 0.999) = INTEGER(5.999) = 5  ✓
  value = -3.5 → FUNCTION INTEGER(-3.5 + 0.999) = INTEGER(-2.501) = -2  ✓

ARRONDI STANDARD:
Préfère toujours COMPUTE ... ROUNDED au lieu de FUNCTION:
  ✅ CORRECT:   COMPUTE result ROUNDED = value1 * value2
  ❌ INTERDIT:  COMPUTE result = FUNCTION ROUND(value1 * value2)

VALIDATION NUMERIC:
Utilise IS NUMERIC ou IS NOT NUMERIC pour tester si un champ alphanumérique contient des chiffres:
  ✅ CORRECT:   IF var IS NUMERIC THEN ...
  ✅ CORRECT:   IF var IS NOT NUMERIC THEN ...
  ❌ INTERDIT:  IF var NUMERIC = FALSE ...

IMPORTANT: Si tu dois arrondir, utilise COMPUTE ROUNDED, PAS FUNCTION ROUND.

ERREUR DE COMPILATION SI TU UTILISES UNE FONCTION INEXISTANTE:
Si tu génères FUNCTION CEILING, CEIL, FLOOR, ROUND, SQRT, POW, etc.,
le compilateur GnuCOBOL retournera: "erreur : FUNCTION « XXX » inconnue"
et la compilation ÉCHOUERA.
"""


def get_business_layer_strict_rules() -> str:
    """
    Get strict rules for BUSINESS layer (presentation only).

    This reinforces that BUSINESS should ONLY display data, no logic.
    Added to prompts when layer == "business".

    Returns:
        Formatted text for prompt inclusion
    """
    return """=== COUCHE BUSINESS : PRÉSENTATION UNIQUEMENT (CRITIQUE) ===
La couche BUSINESS génère UNIQUEMENT de l'affichage (présentation des données).

RÈGLES STRICTES POUR BUSINESS:
1. WORKING-STORAGE SECTION doit être VIDE (aucune variable)
2. UN SEUL paragraphe : celui spécifié dans fonctions (ex: DISPLAY-OPERATION)
3. UNIQUEMENT des instructions DISPLAY
4. UNIQUEMENT le statement obligatoire final (GOBACK)

AUTORISÉ DANS BUSINESS:
- DISPLAY 'texte'
- DISPLAY variable-linkage
- DISPLAY 'label' variable-linkage
- GOBACK (ou STOP RUN si spécifié)

STRICTEMENT INTERDIT DANS BUSINESS:
- ❌ COMPUTE, ADD, SUBTRACT, MULTIPLY, DIVIDE (calculs → LOGIC)
- ❌ IF, ELSE, EVALUATE, WHEN (logique → LOGIC)
- ❌ PERFORM (appels → LOGIC)
- ❌ CALL (appels → LOGIC)
- ❌ MOVE (sauf MOVE ... TO variable de travail, mais pas de variables de travail!)
- ❌ EXEC SQL (SQL → DAL)
- ❌ Variables WORKING-STORAGE (doivent rester vides)
- ❌ Paragraphes multiples (un seul paragraphe autorisé)
- ❌ Logique métier (toutes les règles R1-R7 doivent être en LOGIC)
- ❌ Validations (IF pour contrôler valeurs → LOGIC)

ARCHITECTURE 3 COUCHES:
- DAL (Data Access Layer)    : EXEC SQL, accès base de données
- LOGIC (Orchestration)       : PERFORM, CALL, calculs métier, validations
- BUSINESS (Presentation)     : DISPLAY uniquement

EXEMPLE CORRECT (BUSINESS):
```cobol
DISPLAY-OPERATION.
    DISPLAY '----------------------------------------'
    DISPLAY 'PRODUIT   : ' LK-PROD-NAME
    DISPLAY 'STOCK ACT.: ' LK-STOCK-CURRENT
    DISPLAY 'QTE REAPP.: ' LK-REORDER-QTY
    DISPLAY '----------------------------------------'
    GOBACK
    .
```

EXEMPLE INCORRECT (BUSINESS):
```cobol
DISPLAY-OPERATION.
    PERFORM CALCULATE-REORDER    ← INTERDIT! Pas de PERFORM
    IF LK-STOCK-CURRENT < 0      ← INTERDIT! Pas de validation
        MOVE 'ERROR' TO STATUS   ← INTERDIT! Pas de logique
    END-IF
    COMPUTE REORDER-QTY = ...    ← INTERDIT! Pas de calcul
    DISPLAY 'PRODUIT   : ' LK-PROD-NAME
    GOBACK
    .
```

SI TU GÉNÈRES DE LA LOGIQUE DANS BUSINESS:
- Le code sera rejeté lors de la validation
- Tous les calculs, validations, et appels doivent être dans LOGIC
- BUSINESS reçoit des données déjà calculées via LINKAGE et les affiche

RAPPEL: Si les requirements contiennent des règles métier (R1-R7),
ces règles sont pour la couche LOGIC, PAS pour BUSINESS.
BUSINESS affiche simplement le résultat des calculs effectués par LOGIC.
"""


def get_env_set_method_guide(env_set_method: list) -> str:
    """
    Get guide for PostgreSQL environment variable setting.

    This prevents LLM from hallucinating CALL 'SETENV'.
    Added to DAL prompts when function manipulates env vars.

    Args:
        env_set_method: List of method lines from spec (e.g., ["DISPLAY <ENV_NAME> UPON ENVIRONMENT-NAME"])

    Returns:
        Formatted text for prompt inclusion
    """
    if not env_set_method:
        return ""

    method_lines = "\n".join(f"  {line}" for line in env_set_method)

    return f"""=== MÉTHODE ENV POSTGRESQL (OBLIGATOIRE) ===
Pour définir les variables d'environnement PostgreSQL, utilise UNIQUEMENT:
{method_lines}

EXEMPLE CONCRET pour PGHOST:
  DISPLAY WS-PGHOST-NAME UPON ENVIRONMENT-NAME
  DISPLAY WS-PGHOST-VALUE UPON ENVIRONMENT-VALUE

EXEMPLE COMPLET (toutes les variables PostgreSQL):
  DISPLAY WS-PGHOST-NAME UPON ENVIRONMENT-NAME
  DISPLAY WS-PGHOST-VALUE UPON ENVIRONMENT-VALUE
  DISPLAY WS-PGPORT-NAME UPON ENVIRONMENT-NAME
  DISPLAY WS-PGPORT-VALUE UPON ENVIRONMENT-VALUE
  DISPLAY WS-PGUSER-NAME UPON ENVIRONMENT-NAME
  DISPLAY WS-PGUSER-VALUE UPON ENVIRONMENT-VALUE
  DISPLAY WS-PGPASSWORD-NAME UPON ENVIRONMENT-NAME
  DISPLAY WS-PGPASSWORD-VALUE UPON ENVIRONMENT-VALUE
  DISPLAY WS-PGDATABASE-NAME UPON ENVIRONMENT-NAME
  DISPLAY WS-PGDATABASE-VALUE UPON ENVIRONMENT-VALUE

MOTS-CLÉS COBOL RÉSERVÉS (à ajouter aux identifiants autorisés):
- ENVIRONMENT-NAME  : Mot-clé COBOL pour DISPLAY UPON
- ENVIRONMENT-VALUE : Mot-clé COBOL pour DISPLAY UPON

INTERDICTIONS STRICTES:
- ❌ N'invente PAS 'CALL SETENV' (n'existe pas en COBOL)
- ❌ N'utilise PAS setenv/putenv (fonctions C, pas COBOL)
- ❌ N'utilise PAS MOVE vers des variables système
- ✅ Utilise UNIQUEMENT les instructions DISPLAY ... UPON ci-dessus

ERREUR SI TU INVENTES 'CALL SETENV':
Le compilateur retournera une erreur car cette fonction n'existe pas en COBOL.
La méthode COBOL standard est DISPLAY ... UPON ENVIRONMENT-NAME/VALUE.
"""


def filter_requirements_by_layer(requirements: list, layer: str) -> list:
    """
    Filter requirements based on layer to prevent logic leakage.

    BUSINESS should NOT see business logic rules or validation rules.
    These rules belong to LOGIC layer.

    Args:
        requirements: List of requirement dicts from spec
        layer: Layer name ("business", "logic", "dal")

    Returns:
        Filtered list of requirements appropriate for this layer
    """
    if not requirements:
        return []

    if layer == "business":
        # BUSINESS should only see presentation/technical requirements
        # Exclude business rules and validations (those are for LOGIC)
        filtered = []
        for req in requirements:
            req_type = (req.get("type") or "").lower()
            # Exclude business logic and validation rules
            if req_type not in ["regle_metier", "business_rule", "calcul", "validation"]:
                filtered.append(req)
        return filtered

    elif layer == "dal":
        # DAL should only see data access requirements
        filtered = []
        for req in requirements:
            req_type = (req.get("type") or "").lower()
            # Only include technical/data requirements
            if req_type in ["technique", "technical", "data", "sql"]:
                filtered.append(req)
        return filtered

    else:
        # LOGIC gets all requirements (it's the orchestration layer)
        return requirements


def format_requirements_for_prompt(requirements: list) -> str:
    """
    Format requirements list for prompt inclusion.

    Args:
        requirements: Filtered list of requirements

    Returns:
        Formatted text for prompt
    """
    if not requirements:
        return "Aucune (presentation/technique seulement)"

    lines = []
    for req in requirements:
        req_id = req.get("id", "").strip()
        req_type = req.get("type", "").strip()
        req_rule = req.get("regle", "").strip()
        req_notes = req.get("notes", "").strip()

        if not req_rule:
            continue

        label_parts = []
        if req_id:
            label_parts.append(req_id)
        if req_type:
            label_parts.append(req_type)
        label = "[" + "][".join(label_parts) + "] " if label_parts else ""
        line = f"{label}{req_rule}"
        if req_notes:
            line = f"{line} (notes: {req_notes})"
        lines.append(line)

    return "\n".join(lines) if lines else "Aucune"
