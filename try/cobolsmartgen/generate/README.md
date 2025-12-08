# generate

Generation des artefacts COBOL et SQL a partir des plans calcules.

Fichiers cles :
- `_llm_codegen.py` : fonctions communes d'appel LLM et de mise en forme des reponses.
- `cobol_headers.py` : genere les sections DATA/FILE/WORKING-STORAGE.
- `cobol_procedures.py` : genere les procedures (division PROCEDURE) via LLM.
- `sql_files.py` : construit les requetes SQL pour le SGBD cible.
- `assemble_layout.py` : assemble les morceaux COBOL finalises dans la structure de sortie.
- `code_context_builder.py` : construit le contexte textuel pour guider les prompts.
- `procedure_templates.py` et `prompt_formatters.py` : templates et formatage des prompts LLM.

Etape d'appel : see `cli/main.py` (phases 7-11) pour l'ordre d'execution.
