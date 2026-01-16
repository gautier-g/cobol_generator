## 📋 Project Overview

CobolSmartGen is a project aimed at integrating artificial intelligence into the development cycle to automate the generation of structured COBOL code and database scripts from functional specifications.

Faced with the scarcity of COBOL developers and the increasing complexity of maintaining legacy systems, this project offers a modern solution to assist and accelerate COBOL backend development.

### 🎯 Objectives

- Provide an intelligent code generator capable of assisting COBOL developers in creating reliable backend programs quickly
- Significantly reduce development time and structure COBOL programs
- Modernize the COBOL software production chain by integrating AI technologies

---

## 🔧 Features

The CobolSmartGen generator is capable of:

- **Reading and interpreting** functional specifications organized into sections (Features, Requirements)
- **Automatically generating** COBOL programs organized in three layers:
  - Business layer
  - Application layer
  - Data access layer
- **Producing** corresponding database scripts
- **Proposing** structured, commented code compliant with COBOL standards
- **Allowing** manual adjustments by the developer before validation and deployment

---

## 🏗️ Architecture

The project follows a layered architecture in accordance with COBOL best practices:

```
┌─────────────────────────────────────┐
│   Functional Specifications         │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│     COBOL Code Generator            │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│  Business Layer                     │
├─────────────────────────────────────┤
│  Application Layer                  │
├─────────────────────────────────────┤
│  Data Access Layer                  │
└─────────────────────────────────────┘
```

---

## ⚙️ Technical Constraints

### Data Quality
- Functional specifications must be comprehensive and well-structured
- A stable intermediate format (JSON/DSL) is used between extraction and generation

### Security and Confidentiality
- **Local generation possible (8x22B mistral)**: no external requests

### Validation and Code Quality
- Validation mechanisms (unit tests, integration tests)
- Mandatory human reviews before deployment
- Customization possibility by the developer

### AI Limitations
- Human supervision necessary to validate and correct generations

---

## 👥 Team
### Students
- **FABRE Mathis** - mathis.fabre@telecomnancy.eu
- **LE GLEUT Zoé** - zoe.le-gleut@telecomnancy.eu
- **GEORGEON Gautier** - gautier.georgeon@telecomnancy.eu

---

## 📦 Project Repository

The first version named cobolsmartgen_v1 was initially used with codellama but proved insufficient for our purposes (not precise enough and lack of RAM). That's why it now uses greater LLM APIs (ex: gemini). It generates several automated tests.

The second version named cobolsmartgen_v2 was introduced to simplify the pipeline and reduce the amount of inputs to provide (2000 lines for v1). It works with several programs containing up to 3 database tables and can easily generate up to 300 lines of working code (compiles and does what is asked). This version can be used locally using mistral 8x22B.

---

## 📄 License

Academic Year: 2025–2026  
Academic Project - TELECOM Nancy

---

## 🤝 Contribution

This project is developed as part of an academic project in partnership with Euro-Information-Développement.

For any questions or suggestions, please contact the project team.