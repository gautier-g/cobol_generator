"""
RAG Retriever module for querying the index with context-aware filtering.
"""

import logging
from pathlib import Path
from typing import Dict, List, Any, Optional

from . import indexer

logger = logging.getLogger(__name__)


def retrieve(context: Dict, k: int = 6) -> List[Dict]:
    """
    Retrieve relevant chunks from the index based on context.
    
    Args:
        context: Context dictionary with program_id, layer, dialect, etc.
        k: Number of results to retrieve
        
    Returns:
        List of relevant chunks with metadata
        
    Example context:
        {
            "program_id": "EMPLOYEE-LOGIC",
            "layer": "logic", 
            "dialecte": "gnucobol",
            "sgbd": "postgres",
            "need": "procedure"
        }
    """
    # Build query from context
    query = build_query(context)
    
    # Build filters from context
    filters = build_filters(context)
    
    # Search the index
    results = indexer.search(query, k=k, filters=filters)
    
    # Post-process results
    results = post_process_results(results, context)
    
    # Deduplicate
    results = deduplicate_results(results)
    
    # Limit to k results
    return results[:k]


def build_query(context: Dict) -> str:
    """
    Build search query from context.
    
    Args:
        context: Context dictionary
        
    Returns:
        Search query string
    """
    query_parts = []
    
    # Add program/entity information
    if 'program_id' in context:
        # Extract entity name from program ID (e.g., EMPLOYEE-LOGIC -> EMPLOYEE)
        parts = context['program_id'].split('-')
        if parts:
            query_parts.append(parts[0])
    
    # Add layer information
    if 'layer' in context:
        query_parts.append(context['layer'])
    
    # Add specific need
    if 'need' in context:
        need = context['need']
        if need == 'procedure':
            query_parts.extend(['procedure', 'PROCEDURE', 'PERFORM'])
        elif need == 'header':
            query_parts.extend(['IDENTIFICATION', 'DIVISION', 'header'])
        elif need == 'data':
            query_parts.extend(['DATA', 'DIVISION', 'WORKING-STORAGE'])
        elif need == 'sql':
            query_parts.extend(['SQL', 'SELECT', 'INSERT', 'UPDATE', 'DELETE'])
        elif need == 'calculation':
            query_parts.extend(['COMPUTE', 'calculation', 'formula'])
        elif need == 'validation':
            query_parts.extend(['validate', 'validation', 'check'])
        else:
            query_parts.append(need)
    
    # Add dialect information
    if 'dialecte' in context:
        dialect = context['dialecte']
        if dialect == 'gnucobol':
            query_parts.extend(['GnuCOBOL', 'gnu'])
        elif dialect == 'ibm':
            query_parts.extend(['IBM', 'Enterprise', 'COBOL'])
        elif dialect == 'microfocus':
            query_parts.extend(['Micro Focus', 'MicroFocus'])
    
    # Add SQL target
    if 'sgbd' in context:
        query_parts.append(context['sgbd'])
    
    # Add specific entity or attribute
    if 'entity' in context:
        query_parts.append(context['entity'])
    if 'attribute' in context:
        query_parts.append(context['attribute'])
    
    # Add requirement or functionality
    if 'requirement' in context:
        query_parts.append(context['requirement'])
    if 'functionality' in context:
        query_parts.append(context['functionality'])
    
    return ' '.join(query_parts)


def build_filters(context: Dict) -> Dict:
    """
    Build filters from context.
    
    Args:
        context: Context dictionary
        
    Returns:
        Filters dictionary for index search
    """
    filters = {}
    
    # Filter by layer
    if 'layer' in context:
        layer = context['layer']
        # Allow 'all' layer documents to match any layer
        filters['layer'] = [layer, 'all']
    
    # Filter by dialect if specified
    if 'dialecte' in context and context.get('strict_dialect'):
        filters['dialect'] = context['dialecte']
    
    # Filter by source type
    if 'source_type' in context:
        filters['source'] = context['source_type']
    
    # Filter by entity if specified
    if 'entity' in context and context.get('entity_filter'):
        filters['entity'] = context['entity']
    
    # Filter by requirement type
    if 'req_type' in context:
        filters['req_type'] = context['req_type']
    
    return filters


def post_process_results(results: List[Dict], context: Dict) -> List[Dict]:
    """
    Post-process search results to enhance relevance.
    
    Args:
        results: Raw search results
        context: Context dictionary
        
    Returns:
        Processed results
    """
    processed = []
    
    for result in results:
        # Copy result
        processed_result = result.copy()
        
        # Add relevance boost for exact layer match
        if result.get('metadata', {}).get('layer') == context.get('layer'):
            processed_result['score'] = result.get('score', 0) * 1.2
        
        # Add relevance boost for dialect match
        if result.get('metadata', {}).get('dialect') == context.get('dialecte'):
            processed_result['score'] = result.get('score', 0) * 1.1
        
        # Add context information
        processed_result['context_match'] = {
            'layer': result.get('metadata', {}).get('layer') == context.get('layer'),
            'dialect': result.get('metadata', {}).get('dialect') == context.get('dialecte'),
            'entity': result.get('metadata', {}).get('entity') == context.get('entity')
        }
        
        # Extract relevant snippet
        if 'need' in context:
            snippet = extract_relevant_snippet(result['text'], context['need'])
            if snippet:
                processed_result['snippet'] = snippet
        
        processed.append(processed_result)
    
    # Re-sort by score
    processed.sort(key=lambda x: x.get('score', 0), reverse=True)
    
    return processed


def extract_relevant_snippet(text: str, need: str, max_length: int = 500) -> Optional[str]:
    """
    Extract the most relevant snippet from text based on need.
    
    Args:
        text: Full text
        need: What is needed (procedure, validation, etc.)
        max_length: Maximum snippet length
        
    Returns:
        Relevant snippet or None
    """
    if not text:
        return None
    
    # Define keywords for different needs
    keywords = {
        'procedure': ['PROCEDURE', 'PERFORM', 'CALL', 'SECTION'],
        'validation': ['validate', 'check', 'verify', 'IF', 'WHEN'],
        'calculation': ['COMPUTE', 'MULTIPLY', 'DIVIDE', 'ADD', 'SUBTRACT'],
        'sql': ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'EXEC SQL'],
        'header': ['IDENTIFICATION', 'PROGRAM-ID', 'AUTHOR', 'DATE'],
        'data': ['DATA', 'WORKING-STORAGE', 'PIC', 'VALUE']
    }
    
    need_keywords = keywords.get(need, [need.upper()])
    
    # Find sentences containing keywords
    import re
    sentences = re.split(r'[.!?]\s+', text)
    relevant_sentences = []
    
    for sentence in sentences:
        sentence_upper = sentence.upper()
        if any(kw in sentence_upper for kw in need_keywords):
            relevant_sentences.append(sentence)
            if len(' '.join(relevant_sentences)) > max_length:
                break
    
    if relevant_sentences:
        snippet = ' '.join(relevant_sentences)
        if len(snippet) > max_length:
            snippet = snippet[:max_length] + '...'
        return snippet
    
    # Fallback: return beginning of text
    if len(text) > max_length:
        return text[:max_length] + '...'
    return text


def deduplicate_results(results: List[Dict]) -> List[Dict]:
    """
    Remove duplicate or near-duplicate results.
    
    Args:
        results: List of search results
        
    Returns:
        Deduplicated results
    """
    if not results:
        return results
    
    seen_texts = set()
    deduplicated = []
    
    for result in results:
        # Create a fingerprint of the text (first 100 chars, normalized)
        text = result.get('text', '')
        if len(text) > 100:
            fingerprint = text[:100].lower().replace(' ', '').replace('\n', '')
        else:
            fingerprint = text.lower().replace(' ', '').replace('\n', '')
        
        # Check if we've seen similar text
        if fingerprint not in seen_texts:
            seen_texts.add(fingerprint)
            deduplicated.append(result)
        else:
            # If duplicate, keep the one with higher score
            for i, existing in enumerate(deduplicated):
                existing_text = existing.get('text', '')
                if len(existing_text) > 100:
                    existing_fp = existing_text[:100].lower().replace(' ', '').replace('\n', '')
                else:
                    existing_fp = existing_text.lower().replace(' ', '').replace('\n', '')
                
                if existing_fp == fingerprint:
                    if result.get('score', 0) > existing.get('score', 0):
                        deduplicated[i] = result
                    break
    
    return deduplicated


def retrieve_for_layer(layer: str, entity: str = None, k: int = 6) -> List[Dict]:
    """
    Convenience method to retrieve chunks for a specific layer.
    
    Args:
        layer: Architecture layer (business, logic, dal)
        entity: Optional entity name
        k: Number of results
        
    Returns:
        List of relevant chunks
    """
    context = {
        'layer': layer
    }
    
    if entity:
        context['entity'] = entity
        context['program_id'] = f"{entity}-{layer.upper()}"
    
    return retrieve(context, k)


def retrieve_for_procedure(program_id: str, procedure_type: str, k: int = 6) -> List[Dict]:
    """
    Retrieve chunks relevant for a specific procedure type.
    
    Args:
        program_id: Program identifier
        procedure_type: Type of procedure (calculate, validate, etc.)
        k: Number of results
        
    Returns:
        List of relevant chunks
    """
    # Parse program ID to get entity and layer
    parts = program_id.split('-')
    entity = parts[0] if parts else None
    layer = parts[1].lower() if len(parts) > 1 else 'logic'
    
    context = {
        'program_id': program_id,
        'layer': layer,
        'need': procedure_type,
        'entity': entity
    }
    
    # Add specific context for procedure types
    if procedure_type == 'calculate':
        context['req_type'] = 'calcul'
    elif procedure_type == 'validate':
        context['req_type'] = 'validation'
    elif procedure_type in ['select', 'insert', 'update', 'delete']:
        context['req_type'] = 'dal'
        context['need'] = 'sql'
    
    return retrieve(context, k)


def retrieve_examples(need: str, dialect: str = None, k: int = 3) -> List[Dict]:
    """
    Retrieve example code snippets.
    
    Args:
        need: What kind of examples (procedure, sql, etc.)
        dialect: Optional COBOL dialect
        k: Number of examples
        
    Returns:
        List of example chunks
    """
    context = {
        'need': need,
        'source_type': 'prompt'  # Examples often come from prompt templates
    }
    
    if dialect:
        context['dialecte'] = dialect
    
    return retrieve(context, k)