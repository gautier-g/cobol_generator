import os
import argparse
from mistralai import Mistral

def ask_and_save(file_in, file_out):
    api_key = 'Y4rxXqqWMv6m5haItVW1IpagkcHoyYdb'
    
    if not api_key:
        raise ValueError("La clé API MISTRAL_API_KEY n'est pas définie dans les variables d'environnement")
    
    print(f"Lecture du fichier '{file_in}'...")
    with open(file_in, "r", encoding="utf-8") as f:
        prompt = f.read()
    
    client = Mistral(api_key=api_key)
    
    print("Envoi de la requête à l'API Mistral...")
    response = client.chat.complete(
        model="mistral-large-latest",
        messages=[
            {
                "role": "user",
                "content": prompt
            }
        ]
    )
    
    contenu_reponse = response.choices[0].message.content
    
    with open(file_out, "w", encoding="utf-8") as f:
        f.write(contenu_reponse)
    
    print(f"✓ Réponse sauvegardée dans '{file_out}'")
    return contenu_reponse

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Envoie le contenu d'un fichier à l'API Mistral et sauvegarde la réponse"
    )
    parser.add_argument(
        "input_file",
        help="Chemin du fichier contenant le prompt"
    )
    parser.add_argument(
        "output_file",
        help="Chemin du fichier où sauvegarder la réponse"
    )
    
    args = parser.parse_args()
    
    try:
        ask_and_save(args.input_file, args.output_file)
    except FileNotFoundError:
        print(f"Erreur: Le fichier '{args.input_file}' n'existe pas")
    except Exception as e:
        print(f"Erreur: {e}")