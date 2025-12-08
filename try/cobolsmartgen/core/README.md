# core

Noyau metier pour assembler les contrats et structures partagees par le pipeline.

Fichier cle :
- `contract_generator.py` : construit les structures de contrat (programmes, tables, IO) consommees par les phases de generation COBOL/SQL.

Import typique : `from cobolsmartgen.core import contract_generator` pour reutiliser les helpers de construction.
