import argparse
import subprocess
from mistralai import Mistral

def generate_cobol(puml_filename, database_operations_string, display_operations_string, basic_functionality_string):
    api_key = 'Y4rxXqqWMv6m5haItVW1IpagkcHoyYdb' # next we'll use Mixtral 8x22B locally
    
    if not api_key:
        raise ValueError("La clé API MISTRAL n'est pas valide.")
    
    print(f"Initialisation du client mistral...")
    client = Mistral(api_key=api_key)
    
    # OUVERTURE DES TEMPLATES
    print(f"Ouverture du fichier input-dal.txt...")
    with open('input-dal.txt', "r", encoding="utf-8") as f:
        dal_input = f.read()
        
    print(f"Ouverture du fichier input-business.txt...")
    with open('input-business.txt', "r", encoding="utf-8") as f:
        business_input = f.read()
        
    print(f"Ouverture du fichier input-logic.txt...")
    with open('input-logic.txt', "r", encoding="utf-8") as f:
        logic_input = f.read()    
    
    print(f"Ouverture du fichier '{puml_filename}'...")
    with open(puml_filename, "r", encoding="utf-8") as f:
        class_diagram_string = f.read()
    
    
    # CRÉATION DU FICHIER DAL
    dal_input = dal_input.replace("puml_diagram", class_diagram_string)
    dal_input = dal_input.replace("database_operations", database_operations_string)
    
    print("Envoi de la requête pour la couche physique...")
    dal_response = client.chat.complete(
        model="mistral-large-latest",
        messages=[
            {
                "role": "user",
                "content": dal_input
            }
        ]
    ).choices[0].message.content
    
    print(f"✓ Création du fichier DAL.cbl...")
    with open('DAL.cbl', "w", encoding="utf-8") as f:
        f.write(dal_response)
        
    with open('DAL.cbl', "r", encoding="utf-8") as f:    
        dal_data = f.read()
    
    
    # CRÉATION DU FICHIER BUSINESS
    business_input = business_input.replace("physical_layer", dal_data)
    business_input = business_input.replace("display_operations", display_operations_string)
    
    print("Envoi de la requête pour la couche business...")
    business_response = client.chat.complete(
        model="mistral-large-latest",
        messages=[
            {
                "role": "user",
                "content": business_input
            }
        ]
    ).choices[0].message.content
    
    print(f"✓ Création du fichier BUSINESS.cbl...")
    with open('BUSINESS.cbl', "w", encoding="utf-8") as f:
        f.write(business_response)
    
    with open('BUSINESS.cbl', "r", encoding="utf-8") as f:
        business_data = f.read()
    
    
    # CRÉATION DU FICHIER LOGIC
    logic_input = logic_input.replace("business_layer", business_data)
    logic_input = logic_input.replace("physical_layer", dal_data)
    logic_input = logic_input.replace("basic_functionality", basic_functionality_string)
    
    print("Envoi de la requête pour la couche logique...")
    business_response = client.chat.complete(
        model="mistral-large-latest",
        messages=[
            {
                "role": "user",
                "content": logic_input
            }
        ]
    ).choices[0].message.content
    
    print(f"✓ Création du fichier LOGIC.cbl...")
    with open('LOGIC.cbl', "w", encoding="utf-8") as f:
        f.write(business_response)
    
    
    #EXÉCUTION DE LA PRÉCOMPILATION
    compute_bash_command(['ocesql', 'DAL.cbl'])
    compute_bash_command(['ocesql', 'BUSINESS.cbl'])
    compute_bash_command(['ocesql', 'LOGIC.cbl'])
    
    
    #EXÉCUTION DE LA COMPILATION
    compute_bash_command(["cobc",
                          "-x",
                          "-o",
                          "EMPLOYEE",
                          "preeqlLOGIC.cob",
                          "preeqlBUSINESS.cob",
                          "preeqlDAL.cob",
                          "-I/usr/local/share/open-cobol-esql/copy",
                          "-L/usr/local/lib",
                          "-locesql",
                          "-lpq"])
    
    return

def compute_bash_command(cmd_array):
    try:
        result = subprocess.run(
            cmd_array,
            capture_output=True,  
            text=True,            
            check=True,           
            timeout=10            
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
        parser.add_argument('puml_filename')
        parser.add_argument('database_operations')
        parser.add_argument('display_operations')
        parser.add_argument('basic_functionality')
        
        args = parser.parse_args()
        
        generate_cobol(args.puml_filename, 
                       args.database_operations, 
                       args.display_operations, 
                       args.basic_functionality)
        
    except SystemExit as e:
        # argparse appelle sys.exit() en cas d'erreur
        if e.code != 0:
            print("\n❌ Arguments invalides. Utilisez --help pour voir l'usage.")
        raise
    except Exception as e:
        print(f"❌ Erreur inattendue: {e}")
        sys.exit(1)