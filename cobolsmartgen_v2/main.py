import sys
import os
import argparse
import subprocess
import yaml
from mistralai import Mistral

def generate_cobol(yaml_filename):
    api_key = os.getenv('MISTRAL_API_KEY') # next we'll use Mixtral 8x22B locally
    
    if not api_key:
        raise ValueError("La clé API MISTRAL n'est pas valide.")
    
    # INITIALISATION DES VARIABLES ET DE MISTRAL
    with open(yaml_filename, "r", encoding="utf-8") as file:
        input_data = yaml.safe_load(file)

    generation_name = input_data["generation_name"]
    class_diagram_string = input_data["puml_diagram"]
    database_operations_string = input_data["database_operations_string"]
    display_operations_string = input_data["layer_operations_string"]
    basic_functionality_string = input_data["basic_functionality_string"]
    
    print(f"Initialisation du client mistral...")
    client = Mistral(api_key=api_key)
    
    
    # OUVERTURE DES TEMPLATES
    print(f"Ouverture du fichier input-dal.txt...")
    with open('dal-template.txt', "r", encoding="utf-8") as f:
        dal_template = f.read()
        
    print(f"Ouverture du fichier input-business.txt...")
    with open('business-template.txt', "r", encoding="utf-8") as f:
        business_template = f.read()
        
    print(f"Ouverture du fichier input-logic.txt...")
    with open('logic-template.txt', "r", encoding="utf-8") as f:
        logic_template = f.read()
    
    
    # CRÉATION DU FICHIER DAL
    dal_template = dal_template.replace("puml_diagram", class_diagram_string)
    dal_template = dal_template.replace("database_operations", database_operations_string)
    
    print("Envoi de la requête pour la couche physique...")
    dal_response = client.chat.complete(
        model="mistral-large-latest",
        messages=[
            {
                "role": "user",
                "content": dal_template
            }
        ]
    ).choices[0].message.content
    
    os.makedirs('generations/' + generation_name, exist_ok=True)
    
    print(f"✓ Création du fichier DAL.cbl...")
    with open('generations/' + generation_name + '/DAL.cbl', "w", encoding="utf-8") as f:
        f.write(dal_response)
        
    with open('generations/' + generation_name + '/DAL.cbl', "r", encoding="utf-8") as f:    
        dal_data = f.read()
    
    
    # CRÉATION DU FICHIER BUSINESS
    business_template = business_template.replace("physical_layer", dal_data)
    business_template = business_template.replace("display_operations", display_operations_string)
    
    print("Envoi de la requête pour la couche business...")
    business_response = client.chat.complete(
        model="mistral-large-latest",
        messages=[
            {
                "role": "user",
                "content": business_template
            }
        ]
    ).choices[0].message.content
    
    print(f"✓ Création du fichier BUSINESS.cbl...")
    with open('generations/' + generation_name + '/BUSINESS.cbl', "w", encoding="utf-8") as f:
        f.write(business_response)
    
    with open('generations/' + generation_name + '/BUSINESS.cbl', "r", encoding="utf-8") as f:
        business_data = f.read()
    
    
    # CRÉATION DU FICHIER LOGIC
    logic_template = logic_template.replace("business_layer", business_data)
    logic_template = logic_template.replace("physical_layer", dal_data)
    logic_template = logic_template.replace("basic_functionality", basic_functionality_string)
    
    print("Envoi de la requête pour la couche logique...")
    logic_response = client.chat.complete(
        model="mistral-large-latest",
        messages=[
            {
                "role": "user",
                "content": logic_template
            }
        ]
    ).choices[0].message.content
    
    print(f"✓ Création du fichier LOGIC.cbl...")
    with open('generations/' + generation_name + '/LOGIC.cbl', "w", encoding="utf-8") as f:
        f.write(logic_response)
    
    gen_dir = os.path.join("generations", generation_name)

    #EXÉCUTION DE LA PRÉCOMPILATION
    compute_bash_command(['ocesql', 'DAL.cbl'], cwd=gen_dir)
    compute_bash_command(['ocesql', 'BUSINESS.cbl'], cwd=gen_dir)
    compute_bash_command(['ocesql', 'LOGIC.cbl'], cwd=gen_dir)
    
    
    #EXÉCUTION DE LA COMPILATION
    compute_bash_command(["cobc",
                          "-x",
                          "-o", 
                          generation_name,
                          "preeqlLOGIC.cob",
                          "preeqlBUSINESS.cob",
                          "preeqlDAL.cob",
                          "-I/usr/local/share/open-cobol-esql/copy",
                          "-L/usr/local/lib",
                          "-locesql",
                          "-lpq"], cwd=gen_dir)
    
    return

def compute_bash_command(cmd_array, cwd=None):
    try:
        result = subprocess.run(
            cmd_array,
            capture_output=True,  
            text=True,            
            check=True,           
            timeout=10,
            encoding="latin-1",
            cwd=cwd
        )
        
        print("✅ Succès!")
        print(result.stdout)
        
    except subprocess.CalledProcessError as e:
        print(f"❌ Erreur (code {e.returncode}):")
        print(e.stderr)
    except subprocess.TimeoutExpired:
        print("⏱️ Timeout dépassé")
    
    return

if __name__ == "__main__":
    try:
        parser = argparse.ArgumentParser()
        parser.add_argument('yaml_filename')
        
        args = parser.parse_args()
        
        generate_cobol(args.yaml_filename)
        
    except SystemExit as e:
        # argparse appelle sys.exit() en cas d'erreur
        if e.code != 0:
            print("\n❌ Arguments invalides. Utilisez --help pour voir l'usage.")
        raise
    except Exception as e:
        print(f"❌ Erreur inattendue: {e}")
        sys.exit(1)