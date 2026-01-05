# rag

RAG index creation and retrieval for prompt enrichment.

## Files

### indexer.py
Purpose: build a local BM25/TF-IDF index from spec, prompts, and config.
Inputs: out/normalized_spec.json, prompts/*.txt, config/project.yaml, config/mapping_types.yaml.
Outputs: out/rag_index/index.json and out/rag_index/metadata.json.
Exchanges: used by retriever and can be queried by prompt builders.

### retriever.py
Purpose: query the local index with context filters.
Inputs: context dict (program_id, layer, dialecte, sgbd, need).
Outputs: ranked list of text snippets with metadata.
Exchanges: calls indexer.search and post-processes results.

### __init__.py
Purpose: package marker (no runtime logic).
