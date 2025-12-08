# Utilisation de l'adaptateur Mistral API

## Configuration

Pour utiliser Mistral comme provider LLM dans CobolSmartGen, il y a deux méthodes :

### Méthode 1 : Variable d'environnement (recommandée)

```bash
export CSG_LLM_PROVIDER=mistral
export MISTRAL_API_KEY="SNCDXcbUtpbN4NZ5nwMo6U322cBsSIf3"
```

### Méthode 2 : Configuration dans le code

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

## Variables d'environnement disponibles

- `CSG_LLM_PROVIDER=mistral` - Active le provider Mistral
- `MISTRAL_API_KEY` - Clé API Mistral (par défaut: "SNCDXcbUtpbN4NZ5nwMo6U322cBsSIf3")
- `MISTRAL_BASE_URL` - URL de base de l'API (par défaut: "https://api.mistral.ai/v1")
- `MISTRAL_MODEL` - Modèle à utiliser (par défaut: "mistral-large-latest")
- `MISTRAL_TEMPERATURE` - Température (par défaut: 0.2)
- `MISTRAL_MAX_TOKENS` - Nombre max de tokens (par défaut: 2560)
- `MISTRAL_TIMEOUT_S` - Timeout en secondes (par défaut: 600)

## Modèles disponibles

- `mistral-large-latest` - Alias de Mistral Large 3 (recommandé)
- `mistral-small-latest` - Version plus rapide et économique
- `codestral-latest` - Optimisé pour le code

## Exemple d'utilisation en Python

### Exemple 1 : Génération simple

```python
from cobolsmartgen.adapters import llm_auto

# Configurer le provider Mistral
config = {
    "llm": {
        "provider": "mistral"
    }
}

# Générer du code COBOL
prompt = "Écris un programme COBOL qui lit un fichier et affiche son contenu."
result = llm_auto.generate(prompt, config=config)
print(result)
```

### Exemple 2 : Chat avec messages

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
    {"role": "user", "content": "Corrige cette erreur de compilation COBOL: ..."}
]

response = llm_auto.chat(messages, config=config)
print(response)
```

### Exemple 3 : Avec prompt système

```python
from cobolsmartgen.adapters import llm_auto

config = {"llm": {"provider": "mistral"}}

system_prompt = "Tu es un générateur de code COBOL professionnel."
user_prompt = "Génère un programme COBOL pour calculer la somme de deux nombres."

result = llm_auto.generate(
    user_prompt,
    system=system_prompt,
    config=config
)
print(result)
```

## Exemple en cURL (pour tester directement)

```bash
curl https://api.mistral.ai/v1/chat/completions \
  -X POST \
  -H "Authorization: Bearer SNCDXcbUtpbN4NZ5nwMo6U322cBsSIf3" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "mistral-large-latest",
    "messages": [
      {
        "role": "user",
        "content": "Écris un programme COBOL Hello World"
      }
    ],
    "max_tokens": 512,
    "temperature": 0.2
  }'
```

## Comparaison avec les autres providers

| Provider | Avantages | Quand l'utiliser |
|----------|-----------|------------------|
| **Mistral** | Modèle puissant (Large 3), bonne qualité, API simple | Production, génération complexe |
| **Groq** | Très rapide, bon pour le prototypage | Développement rapide, tests |
| **Ollama** | Local, gratuit, pas de clé API nécessaire | Développement offline, confidentialité |

## Anciens modes (conservés en commentaire)

L'ancien système de routing entre Groq et Ollama a été conservé en commentaire dans le fichier `llm_auto.py`.
Il continue de fonctionner si vous utilisez `CSG_LLM_PROVIDER=groq` ou si Mistral n'est pas sélectionné.

## Dépannage

### Erreur "HTTP 401 Unauthorized"
- Vérifiez que votre clé API est correcte
- Assurez-vous que la variable `MISTRAL_API_KEY` est bien définie

### Timeout
- Augmentez `MISTRAL_TIMEOUT_S` si les requêtes sont trop longues
- Réduisez `max_tokens` pour accélérer la génération

### Erreur "Model not found"
- Vérifiez que vous utilisez un modèle valide (ex: `mistral-large-latest`)
- Consultez la liste des modèles via: `curl https://api.mistral.ai/v1/models -H "Authorization: Bearer <key>"`

## Documentation officielle

- [Quickstart Mistral](https://docs.mistral.ai/getting-started/quickstart/)
- [Chat Completions API](https://docs.mistral.ai/api/#tag/chat)
- [API Reference](https://docs.mistral.ai/api/)
- [SDK Python](https://docs.mistral.ai/getting-started/clients/)
