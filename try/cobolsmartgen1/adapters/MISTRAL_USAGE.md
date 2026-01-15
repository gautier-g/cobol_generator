# Guide d'utilisation de Mistral API

## Configuration

### Methode 1 : Variables d'environnement (recommandee)

```bash
export CSG_LLM_PROVIDER=mistral
export MISTRAL_API_KEY="<VOTRE_CLE_API>"
```

### Methode 2 : Configuration dans le code

```python
config = {
    "llm": {
        "provider": "mistral",
        "model": "mistral-large-latest",
        "temperature": 0.2,
        "max_tokens": 2560
    }
}
```

## Variables d'environnement

| Variable | Description | Defaut |
|----------|-------------|--------|
| `CSG_LLM_PROVIDER` | Active Mistral comme provider | - |
| `MISTRAL_API_KEY` | Cle API Mistral | (obligatoire) |
| `MISTRAL_BASE_URL` | URL de l'API | `https://api.mistral.ai/v1` |
| `MISTRAL_MODEL` | Modele a utiliser | `mistral-large-latest` |
| `MISTRAL_TEMPERATURE` | Temperature (0.0-1.0) | `0.2` |
| `MISTRAL_MAX_TOKENS` | Tokens maximum | `2560` |
| `MISTRAL_TIMEOUT_S` | Timeout en secondes | `600` |

## Modeles disponibles

| Modele | Description | Utilisation |
|--------|-------------|-------------|
| `mistral-large-latest` | Mistral Large 3 | Production (recommande) |
| `mistral-small-latest` | Version compacte | Tests, prototypage |
| `codestral-latest` | Optimise code | Generation de code |

## Exemples d'utilisation

### Generation simple

```python
from cobolsmartgen.adapters import llm_auto

config = {"llm": {"provider": "mistral"}}

prompt = "Genere un programme COBOL Hello World."
result = llm_auto.generate(prompt, config=config)
print(result)
```

### Chat avec messages

```python
from cobolsmartgen.adapters import llm_auto

config = {
    "llm": {
        "provider": "mistral",
        "model": "mistral-large-latest",
        "temperature": 0.1
    }
}

messages = [
    {"role": "system", "content": "Tu es un expert COBOL."},
    {"role": "user", "content": "Explique la PROCEDURE DIVISION."}
]

response = llm_auto.chat(messages, config=config)
print(response)
```

### Avec prompt systeme

```python
from cobolsmartgen.adapters import llm_auto

config = {"llm": {"provider": "mistral"}}

result = llm_auto.generate(
    "Calcule la somme de deux nombres.",
    system="Tu es un generateur COBOL professionnel.",
    config=config
)
print(result)
```

## Test avec cURL

```bash
curl https://api.mistral.ai/v1/chat/completions \
  -X POST \
  -H "Authorization: Bearer <VOTRE_CLE_API>" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "mistral-large-latest",
    "messages": [{"role": "user", "content": "Hello World en COBOL"}],
    "max_tokens": 512,
    "temperature": 0.2
  }'
```

## Comparaison des providers

| Provider | Forces | Usage |
|----------|--------|-------|
| **Mistral** | Qualite, API simple | Production |
| **Groq** | Rapidite | Prototypage |
| **Ollama** | Local, gratuit | Developpement offline |

## Depannage

### HTTP 401 Unauthorized
- Verifiez que `MISTRAL_API_KEY` est correctement definie
- Verifiez que la cle n'a pas expire

### Timeout
- Augmentez `MISTRAL_TIMEOUT_S`
- Reduisez `max_tokens`

### Model not found
- Verifiez le nom du modele
- Listez les modeles disponibles :
  ```bash
  curl https://api.mistral.ai/v1/models \
    -H "Authorization: Bearer <CLE>"
  ```

## Documentation officielle

- [Quickstart](https://docs.mistral.ai/getting-started/quickstart/)
- [API Reference](https://docs.mistral.ai/api/)
- [SDK Python](https://docs.mistral.ai/getting-started/clients/)
