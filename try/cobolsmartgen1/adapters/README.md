# adapters - Clients LLM et compilation

Ce module fournit les **adaptateurs** pour communiquer avec les differents providers LLM (Mistral, Groq, Gemini, Ollama) et pour compiler le code COBOL genere.

## Role dans le pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                        GENERATE MODULE                              │
│                  (cobol_procedures.py, etc.)                        │
└─────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                          ADAPTERS                                   │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌─────────────┐                                                    │
│  │  llm_auto   │  ◄── Routeur vers le provider actif                │
│  └──────┬──────┘                                                    │
│         │                                                           │
│         ├──────────┬──────────┬──────────┐                         │
│         ▼          ▼          ▼          ▼                         │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐               │
│  │ Mistral  │ │  Groq    │ │ Gemini   │ │ Ollama   │               │
│  │  (API)   │ │  (API)   │ │  (API)   │ │ (local)  │               │
│  └──────────┘ └──────────┘ └──────────┘ └──────────┘               │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Fichiers

### llm_auto.py

**Fonction principale** : Router les appels LLM vers le provider configure.

| Aspect | Description |
|--------|-------------|
| **Entrees** | Prompt ou messages, config["llm"], timeout |
| **Sorties** | Reponse texte du LLM |
| **Config** | `CSG_LLM_PROVIDER` ou `config.llm.provider` |

#### Selection du provider

```python
# Ordre de priorite
1. Variable d'environnement CSG_LLM_PROVIDER
2. config["llm"]["provider"] dans project.yaml
3. Defaut: mistral
```

### llm_mistral.py

**Fonction** : Client HTTP pour l'API Mistral.

| Variable d'environnement | Description | Defaut |
|--------------------------|-------------|--------|
| `MISTRAL_API_KEY` | Cle API Mistral | (obligatoire) |
| `MISTRAL_MODEL` | Modele a utiliser | `mistral-large-latest` |
| `MISTRAL_TEMPERATURE` | Temperature de generation | `0.2` |
| `MISTRAL_MAX_TOKENS` | Tokens maximum | `2560` |
| `MISTRAL_TIMEOUT_S` | Timeout en secondes | `600` |

#### Modeles recommandes

| Modele | Usage |
|--------|-------|
| `mistral-large-latest` | Production (recommande) |
| `codestral-latest` | Optimise pour le code |
| `mistral-small-latest` | Rapide et economique |

### llm_groq.py

**Fonction** : Client HTTP pour l'API Groq (style OpenAI).

| Variable d'environnement | Description |
|--------------------------|-------------|
| `GROQ_API_KEY` | Cle API Groq |
| `GROQ_MODEL` | Modele (ex: `llama-3.1-70b-versatile`) |
| `GROQ_TIMEOUT_S` | Timeout en secondes |

### llm_ollama.py

**Fonction** : Client pour Ollama (LLM local).

| Variable d'environnement | Description | Defaut |
|--------------------------|-------------|--------|
| `OLLAMA_HOST` | URL du serveur Ollama | `http://localhost:11434` |
| `OLLAMA_MODEL` | Modele local | `codellama` |

**Avantage** : Pas de cle API, fonctionne offline, donnees confidentielles.

## Tracage des appels LLM

Tous les appels sont traces dans `out/trace/prompts/<provider>/` :

```
out/trace/prompts/mistral/
└── 20260115/
    ├── 143052_chat.prompt.txt    # Prompt envoye
    ├── 143052_chat.response.txt  # Reponse recue
    └── 143052_chat.meta.json     # Metadonnees (tokens, duree)
```

## Comparaison des providers

| Provider | Avantages | Inconvenients |
|----------|-----------|---------------|
| **Mistral** | Haute qualite, bon pour COBOL | Payant, necessite API |
| **Groq** | Tres rapide, gratuit (limites) | Qualite variable |
| **Gemini** | Gratuit (quotas), multimodal | Latence variable |
| **Ollama** | Gratuit, local, offline | Necessite GPU/CPU puissant |

## Points cles pour la soutenance

1. **Architecture modulaire** : Chaque provider est un adaptateur independant
2. **Routage automatique** : `llm_auto.py` simplifie l'integration
3. **Tracabilite** : Tous les echanges sont enregistres
4. **Flexibilite** : Changement de provider sans modifier le code
