# COBOL Smart Generator v2

Automatic multi-layer COBOL code generator using Mistral AI to create complete COBOL applications with PostgreSQL database access.

## 📋 Description

This project automatically generates structured COBOL code in three layers from YAML specifications:
- **DAL (Data Access Layer)**: PostgreSQL data access layer
- **BUSINESS**: Business layer with display logic
- **LOGIC**: Main application logic layer

The generator uses the Mistral AI API to produce COBOL code following best practices, then automatically precompiles and compiles it.

## 🔄 Generation Process

1. **Loading**: Read input YAML file
2. **Initialization**: Connect to Mistral API
3. **DAL Generation**: Create data access layer
4. **BUSINESS Generation**: Create business layer
5. **LOGIC Generation**: Create logic layer
6. **Precompilation**: OCESQL processing of `.cbl` files → `.cob`
7. **Compilation**: GnuCOBOL compilation to executable

## 📁 Project Structure
```
cobolsmartgen_v2/
├── main.py                      # Main generation script
├── dal-template.txt             # DAL layer template
├── business-template.txt        # Business layer template
├── logic-template.txt           # Logic layer template
├── base_input.yaml              # Example input file
├── mcd_diagram.puml             # PlantUML MCD diagram for syntax checking
├── plantuml-1.2025.7.jar        # PlantUML JAR
│
├── inputs/                      # Input YAML files
│   ├── basic_tests/            # Basic tests (customer, department, invoice, etc.)
│   └── carbontrack_tests/      # CarbonTrack tests (antenne, hebergement, etc.)
│
├── json_db_imports/             # JSON files for DB import
│
└── generations/                 # Generated COBOL code
    ├── customer/
    │   ├── customer             # Compiled executable
    │   ├── DAL.cbl             # DAL source code
    │   ├── BUSINESS.cbl        # Business source code
    │   ├── LOGIC.cbl           # Logic source code
    │   ├── preeqlDAL.cob       # Precompiled DAL
    │   ├── preeqlBUSINESS.cob  # Precompiled Business
    │   └── preeqlLOGIC.cob     # Precompiled Logic
    └── ...
```

## 📦 Project Setup

### 1. Python Setup
- Install Python 3.13.11 (or 3.9+)
```bash
python3 -m venv venv
```
```bash
source venv/bin/activate
```
```bash
pip install -r requirements.txt
```
- Quit whenever you want with the command: `deactivate`

### 2. OCESQL and GnuCOBOL Installation
- Get OCESQL from: https://github.com/opensourcecobol/Open-COBOL-ESQL/releases/tag/v1.4
- Install it
- Install GnuCOBOL by running: `sudo apt install cobc`

### 3. Setup Environment Variables
```bash
export MISTRAL_API_KEY="your_key"
export COB_LDFLAGS='-Wl,--no-as-needed'
```

### 4. Set PostgreSQL Credentials
In `dal-template.txt`, configure your database connection:
```
La base de données est nommée '<db_name>', et il faut s'y connecter avec l'utilisateur '<db_user>' et le mot de passe '<db_password>'.
```

## 🧪 Class Diagram Testing
- Edit `mcd_diagram.puml`
- Install Java 21
```bash
java -jar plantuml-1.2025.7.jar mcd_diagram.puml
```

## 🚀 Basic Use

### COBOL Code Generation
```bash
python main.py <yaml_file>
```

The resulting files are generated in `generations/<generation_name>`.

### Run Generated Program
```bash
./generations/<generation_name>/<generation_name>
```

## 📝 Input YAML File Format

The YAML file must contain the following fields:
```yaml
generation_name: "module_name"

puml_diagram: |
  @startuml
  class ClassName {
    +attribute1: Type
    +attribute2: Type
  }
  @enduml

database_operations_string: |
  Description of CRUD operations on the database
  (CREATE, READ, UPDATE, DELETE)

layer_operations_string: |
  Description of display operations
  (menus, forms, reports)

basic_functionality_string: |
  Description of the main functionality
  of the module to generate
```

### Example:
```yaml
generation_name: base_example

puml_diagram: |
  @startuml
  class Employee {
    EMP_ID : integer (needed)
    EMP_NAME : varchar(30) (needed)
    SALARY_BRUT : numeric(8,2) (needed)
    SALARY_NET : numeric(8,2) (optional)
  }
  @enduml

database_operations_string: |
  Donne moi un programme COBOL qui permet de parcourir une telle table
  et qui fournit des procédures pour sauvegarder un élément
  ou lire un élément.

layer_operations_string: |
  Donne moi un programme COBOL qui permet d'afficher un élément EMPLOYEE.

basic_functionality_string: |
  Donne moi un programme COBOL qui parcours les éléments de la table employee
  et qui calculent la valeur du salaire net pour chaque
  puis sauvegarde dans la base de données
  (avec un affichage pour chaque employé itéré).
```

## ⚠️ Error Handling
Compilation and precompilation errors are displayed with:
- ✅ Success
- ❌ Error with return code
- ⏱️ Timeout exceeded

## 🛠️ Manual Compilation

The script uses the following compilation parameters:
```bash
ocesql LOGIC.cbl
```
```bash
ocesql DAL.cbl
```
```bash
ocesql BUSINESS.cbl
```
```bash
cobc -x -o <name> \
  preeqlLOGIC.cob \
  preeqlBUSINESS.cob \
  preeqlDAL.cob \
  -I/usr/local/share/open-cobol-esql/copy \
  -L/usr/local/lib \
  -locesql \
  -lpq
```

## 🤝 Contribution

This project uses a modular architecture with customizable templates. To add new features:

1. Modify the templates (`dal-template.txt`, `business-template.txt`, `logic-template.txt`)
2. Add new YAML files in `inputs/`
3. Test generation and compilation

## 👤 Author

GEORGEON Gautier