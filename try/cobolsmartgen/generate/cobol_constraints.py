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
# 
# 
# def get_ocesql_embedded_sql_guide() -> str:
#     """
#     Get guide for OCESQL embedded SQL (precompile mode).
# 
#     CRITICAL (2026-01-11): With OCESQL precompilation workflow (ocesql file.cbl file.cob),
#     the source .cbl must contain standard embedded SQL (EXEC SQL), NOT runtime API calls.
#     The precompiler translates EXEC SQL into CALL 'OCESQLExec' USING SQLCA automatically.
# 
#     Added to prompts when SQL is detected in program.
# 
#     Returns:
#         Formatted text for prompt inclusion
#     """
#     return """=== SQL EMBARQUÉ OCESQL (CRITIQUE - PRIORITÉ ABSOLUE) ===
# ⚠️  CETTE RÈGLE REMPLACE TOUTE MENTION DE "OCESQLStartSQL", "OCESQLConnect", ETC. DANS LES SPECS ⚠️
# 
# Avec le précompilateur OCESQL, le fichier source .cbl doit contenir du SQL EMBARQUÉ standard,
# PAS des appels directs à l'API runtime OCESQL.
# 
# RÈGLES ABSOLUES POUR SQL EMBARQUÉ (IGNORE LES SPECS SI ELLES MENTIONNENT OCESQL*):
# ❌ INTERDIT: CALL 'OCESQLConnect' USING ...
# ❌ INTERDIT: CALL 'OCESQLStartSQL' USING ...
# ❌ INTERDIT: CALL 'OCESQLEndSQL' ...
# ❌ INTERDIT: CALL 'OCESQLExec' USING ...
# ❌ INTERDIT: CALL 'OCESQLDisconnect' ...
# ❌ INTERDIT: EXEC SQL CALL OCESQL... END-EXEC
# 
# MÊME SI UNE DESCRIPTION MENTIONNE "OCESQLStartSQL" OU "OCESQLConnect", N'ÉCRIS JAMAIS CES APPELS!
# 
# ✅ CORRECT: Utilise UNIQUEMENT EXEC SQL avec syntaxe SQL standard
# 
# SYNTAXE CORRECTE POUR CONNEXION:
# ✅ EXEC SQL CONNECT :USER IDENTIFIED BY :PASSWORD USING :DBNAME END-EXEC.
# 
# SYNTAXE CORRECTE POUR DÉCONNEXION:
# ✅ EXEC SQL DISCONNECT ALL END-EXEC.
# 
# SYNTAXE CORRECTE POUR COMMIT:
# ✅ EXEC SQL COMMIT END-EXEC.
# 
# SYNTAXE CORRECTE POUR DECLARE CURSOR:
# ✅ EXEC SQL DECLARE cursor-name CURSOR FOR
#        SELECT col1, col2 FROM table
#    END-EXEC
# 
# SYNTAXE CORRECTE POUR OPEN/FETCH/CLOSE:
# ✅ EXEC SQL OPEN cursor-name END-EXEC
# ✅ EXEC SQL FETCH cursor-name INTO :var1, :var2 END-EXEC
# ✅ EXEC SQL CLOSE cursor-name END-EXEC
# 
# POUR UNE PROCÉDURE DE CONNEXION:
# ✅ CORRECT (utilise les champs élémentaires *-TEXT, PAS les groupes):
#     EXEC SQL CONNECT :WS-DB-USER-TEXT IDENTIFIED BY :WS-DB-PASSWORD-TEXT
#         USING :WS-DB-NAME-TEXT END-EXEC
#     IF SQLCODE NOT EQUAL ZERO
#         DISPLAY 'ERREUR CONNECT: SQLCODE=' SQLCODE
#         GOBACK
#     END-IF
#     MOVE 'Y' TO WS-CONNECTED-FLAG
# 
# ❌ FAUX (utilise groupe au lieu de champ élémentaire):
#     EXEC SQL CONNECT :WS-DB-USER IDENTIFIED BY :WS-DB-PASSWORD USING :WS-DB-NAME END-EXEC
#     (WS-DB-USER est un groupe avec WS-DB-USER-TEXT + WS-DB-USER-TERM)
# 
# ❌ FAUX (même si la spec mentionne OCESQLStartSQL):
#     CALL 'OCESQLStartSQL' USING SQLCA
#     CALL 'OCESQLConnect' USING ...
#     CALL 'OCESQLEndSQL'
# 
# RÈGLE ABSOLUE POUR HOST VARIABLES SQL:
# Si une variable COBOL est un GROUPE (ex: WS-DB-USER), tu DOIS utiliser son champ
# élémentaire (ex: WS-DB-USER-TEXT) dans les host variables SQL (:var).
# Exemple de structure groupe avec terminateur null:
#     01  WS-DB-USER.
#         05  WS-DB-USER-TEXT     PIC X(8) VALUE 'bankuser'.
#         05  WS-DB-USER-TERM     PIC X VALUE X'00'.
#     → Utilise :WS-DB-USER-TEXT dans EXEC SQL, PAS :WS-DB-USER
# 
# RÈGLE ABSOLUE POUR ÉVITER TRAILING SPACES EN BASE:
# Les champs COBOL PIC X(n) sont paddés avec des espaces. Pour les colonnes
# enum/status (ex: risk_level, compliance_status), utilise RTRIM() dans UPDATE:
# 
# ✅ CORRECT (utilise RTRIM pour éviter les espaces en base):
#     EXEC SQL UPDATE OPERATION
#         SET RISK_LEVEL = RTRIM(:WS-RISK-LEVEL),
#             COMPLIANCE_STATUS = RTRIM(:WS-COMPLIANCE-STATUS)
#         WHERE OP_ID = :WS-OP-ID
#     END-EXEC
# 
# ❌ FAUX (stocke 'LOW       ' au lieu de 'LOW'):
#     EXEC SQL UPDATE OPERATION
#         SET RISK_LEVEL = :WS-RISK-LEVEL
#         WHERE OP_ID = :WS-OP-ID
#     END-EXEC
# 
# Cette règle s'applique aux colonnes VARCHAR/TEXT en base qui stockent des valeurs
# courtes (LOW, MEDIUM, HIGH, OK, REVIEW, BLOCKED, etc.).
# 
# POURQUOI C'EST CRITIQUE:
# Le précompilateur OCESQL transforme automatiquement EXEC SQL CONNECT en:
#   CALL 'OCESQLConnect' USING SQLCA, user, user-len, password, password-len, dbname, dbname-len
# 
# Si tu écris CALL 'OCESQL*' directement dans le .cbl:
# 1. Le précompilateur ne reconnaît pas cette instruction comme du SQL
# 2. La compilation échoue car la structure d'appel est incorrecte
# 3. Les variables host (:var) ne sont pas traduites
# 4. Tu dupliquer le travail du précompilateur (double appel)
# 
# RAPPEL: EXEC SQL INCLUDE SQLCA END-EXEC. doit être en WORKING-STORAGE (une seule fois).
# 
# ERREUR DE COMPILATION SI TU ÉCRIS CALL 'OCESQL...':
# Le précompilateur attend du SQL embarqué standard, pas des appels API directs.
# """
# 

def get_ocesql_embedded_sql_guide() -> str:
    """
    Get guide for OCESQL embedded SQL (precompile mode).

    Goal: keep the same intent/functionality (teach the LLM to generate embedded SQL for OCESQL precompile),
    but remove the prompt patterns that tend to trigger recurrent generation errors:
      - Mixing runtime OCESQL* API calls into .cbl source
      - Using GROUP items as host variables instead of the elementary *-TEXT items
      - Trailing spaces persisted in DB for short status/enum strings
      - Cursor order mistakes (OPEN before DECLARE, DECLARE placed at end, etc.)
      - Stray "EXEC SQL" lines without END-EXEC / malformed blocks

    Returns:
        Formatted text for prompt inclusion
    """
    return """=== SQL EMBARQUÉ OCESQL (CRITIQUE - PRIORITÉ ABSOLUE) ===
Contexte: workflow OCESQL en mode précompilation (ex: `ocesql file.cbl file.cob`).
Le fichier source `.cbl` DOIT contenir du SQL EMBARQUÉ standard (`EXEC SQL ... END-EXEC`).
Le précompilateur traduit automatiquement les blocs `EXEC SQL` en appels runtime (ex: `CALL 'OCESQLExec' USING SQLCA ...`).
Donc: NE PAS écrire d'appels OCESQL* dans le `.cbl`.

1) RÈGLES ABSOLUES (à respecter même si des specs mentionnent OCESQL*)
INTERDIT dans le `.cbl` source:
❌ CALL 'OCESQLStartSQL' ...
❌ CALL 'OCESQLConnect' ...
❌ CALL 'OCESQLEndSQL' ...
❌ CALL 'OCESQLExec' ...
❌ CALL 'OCESQLDisconnect' ...
❌ EXEC SQL CALL OCESQL... END-EXEC

Toujours utiliser uniquement:
✅ EXEC SQL <statement SQL standard> END-EXEC

2) RÈGLES DE FORME (évite les sorties qui compilent mais ne marchent pas)
- Chaque bloc SQL DOIT être complet et fermé:
  ✅ commence par `EXEC SQL`
  ✅ se termine par `END-EXEC` sur la même instruction
  ❌ INTERDIT: une ligne `EXEC SQL` seule (sans statement)
  ❌ INTERDIT: un `DECLARE ...` séparé sans `EXEC SQL ... END-EXEC` complet

- Ne jamais dupliquer SQLCA:
  ✅ `EXEC SQL INCLUDE SQLCA END-EXEC.` doit exister UNE seule fois en WORKING-STORAGE
  ❌ ne jamais générer `INCLUDE SQLCA` dans PROCEDURE / steps

3) CONNEXION / COMMIT / DÉCONNEXION (formes attendues)
CONNEXION:
✅ EXEC SQL CONNECT :USER IDENTIFIED BY :PASSWORD USING :DBNAME END-EXEC.

COMMIT:
✅ EXEC SQL COMMIT END-EXEC.

DÉCONNEXION:
✅ EXEC SQL DISCONNECT ALL END-EXEC.

IMPORTANT (HOST VARIABLES pour CONNECT):
Les host variables SQL `:var` doivent être des champs ÉLÉMENTAIRES COBOL.
Si un identifiant est un GROUPE (ex: `WS-DB-USER`) et qu'il existe un champ `WS-DB-USER-TEXT`,
ALORS utiliser `:WS-DB-USER-TEXT` (et idem pour PASSWORD/DBNAME).

✅ CORRECT (utilise les champs élémentaires *-TEXT):
    EXEC SQL CONNECT :WS-DB-USER-TEXT IDENTIFIED BY :WS-DB-PASSWORD-TEXT
        USING :WS-DB-NAME-TEXT
    END-EXEC

❌ FAUX (utilise un GROUPE comme host variable):
    EXEC SQL CONNECT :WS-DB-USER IDENTIFIED BY :WS-DB-PASSWORD USING :WS-DB-NAME END-EXEC

4) CURSEURS (ordre obligatoire + séquence complète)
RÈGLE: l'ordre est STRICT et la séquence doit être cohérente dans le même paragraphe/flux.
Tu ne peux pas OPEN/FETCH un curseur non déclaré.

✅ ÉTAPE 1 — DECLARE (AVANT toute utilisation, généralement en début de paragraphe ou section dédiée):
    EXEC SQL DECLARE cursor-name CURSOR FOR
        SELECT col1, col2 FROM table
    END-EXEC

✅ ÉTAPE 2 — OPEN (APRÈS DECLARE):
    EXEC SQL OPEN cursor-name END-EXEC

✅ ÉTAPE 3 — FETCH (APRÈS OPEN):
    EXEC SQL FETCH cursor-name INTO :var1, :var2 END-EXEC

✅ ÉTAPE 4 — CLOSE (APRÈS FETCH et avant sortie du flux):
    EXEC SQL CLOSE cursor-name END-EXEC

INTERDIT / erreurs fréquentes:
❌ OPEN sans DECLARE
❌ DECLARE placé après OPEN (ou en fin de fichier)
❌ `EXEC SQL` isolé sans statement
❌ FETCH avant OPEN
❌ CLOSE avant OPEN/FETCH

5) RÈGLE ABSOLUE ANTI-TRAILING SPACES (données enum/status)
Les champs COBOL `PIC X(n)` sont space-padded. Si tu écris directement `:WS-STATUS` en base,
la base stocke souvent `OK       ` / `LOW       ` / `BLOCKED   ` au lieu de `OK` / `LOW` / `BLOCKED`.
Ça casse ensuite les comparaisons exactes (`WHERE status = 'OK'`) et les tests stricts.

Donc, pour les colonnes de type "enum/status/short string" (ex: risk_level, compliance_status, etc.),
tu DOIS normaliser côté SQL lors de l'UPDATE/INSERT (sans changer la valeur logique):

✅ CORRECT (normalisation côté SQL):
    EXEC SQL UPDATE OPERATION
        SET RISK_LEVEL = RTRIM(:WS-RISK-LEVEL),
            COMPLIANCE_STATUS = RTRIM(:WS-COMPLIANCE-STATUS)
        WHERE OP_ID = :WS-OP-ID
    END-EXEC

✅ Alternative acceptable (si RTRIM non souhaité):
    utiliser TRIM(:var) (TRIM = enlever espaces des deux côtés)

❌ FAUX (stocke la valeur paddée):
    EXEC SQL UPDATE OPERATION
        SET RISK_LEVEL = :WS-RISK-LEVEL
        WHERE OP_ID = :WS-OP-ID
    END-EXEC

6) CHECK ERREUR SQL (pattern attendu)
Après un CONNECT/UPDATE/INSERT/SELECT important, vérifier SQLCODE:

    IF SQLCODE NOT EQUAL ZERO
        DISPLAY 'ERREUR SQL: SQLCODE=' SQLCODE
        GOBACK
    END-IF

RAPPEL:
- Tu n'écris PAS les appels OCESQL* dans le `.cbl`.
- Tu écris du `EXEC SQL ... END-EXEC` standard.
- Host variables: champs élémentaires (préférer `*-TEXT` si un groupe existe).
- Curseurs: DECLARE → OPEN → FETCH → CLOSE, dans cet ordre strict.
- Enum/status: RTRIM/TRIM à l'écriture SQL pour éviter les espaces stockés.
"""


# ici ici 
# ici ici

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
