"""
RAG Indexer module for building a local search index.
Implements BM25 and TF-IDF using only standard library.
"""

import json
import logging
import math
import re
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple

from ..utils import fs, trace

logger = logging.getLogger(__name__)


class SimpleIndex:
    """Simple text search index with BM25 scoring."""
    
    def __init__(self, k1: float = 1.5, b: float = 0.75):
        self.k1 = k1  # BM25 parameter
        self.b = b    # BM25 parameter
        self.documents = []
        self.doc_lengths = []
        self.avg_doc_length = 0
        self.doc_count = 0
        self.term_frequencies = defaultdict(lambda: defaultdict(int))
        self.document_frequencies = defaultdict(int)
        self.idf_cache = {}
        
    def add_document(self, doc_id: str, text: str, metadata: Dict = None):
        """Add a document to the index."""
        # Tokenize
        tokens = self._tokenize(text)
        
        # Store document
        self.documents.append({
            'id': doc_id,
            'text': text,
            'metadata': metadata or {},
            'tokens': tokens
        })
        
        # Update statistics
        doc_idx = len(self.documents) - 1
        self.doc_lengths.append(len(tokens))
        self.doc_count += 1
        
        # Update term frequencies
        token_counts = Counter(tokens)
        for term, count in token_counts.items():
            self.term_frequencies[term][doc_idx] = count
            if count > 0:
                self.document_frequencies[term] += 1
        
        # Update average document length
        self.avg_doc_length = sum(self.doc_lengths) / len(self.doc_lengths)
        
        # Clear IDF cache
        self.idf_cache.clear()
    
    def _tokenize(self, text: str) -> List[str]:
        """Simple tokenization."""
        # Convert to lowercase and split on non-alphanumeric
        text = text.lower()
        tokens = re.findall(r'\w+', text)
        return tokens
    
    def _calculate_idf(self, term: str) -> float:
        """Calculate IDF for a term."""
        if term in self.idf_cache:
            return self.idf_cache[term]
        
        df = self.document_frequencies.get(term, 0)
        if df == 0:
            idf = 0
        else:
            # IDF = log((N - df + 0.5) / (df + 0.5))
            idf = math.log((self.doc_count - df + 0.5) / (df + 0.5))
        
        self.idf_cache[term] = idf
        return idf
    
    def _calculate_bm25(self, doc_idx: int, query_tokens: List[str]) -> float:
        """Calculate BM25 score for a document."""
        score = 0.0
        doc_length = self.doc_lengths[doc_idx]
        
        for term in query_tokens:
            if term not in self.term_frequencies:
                continue
            
            tf = self.term_frequencies[term].get(doc_idx, 0)
            if tf == 0:
                continue
            
            idf = self._calculate_idf(term)
            
            # BM25 formula
            numerator = tf * (self.k1 + 1)
            denominator = tf + self.k1 * (1 - self.b + self.b * doc_length / self.avg_doc_length)
            score += idf * (numerator / denominator)
        
        return score
    
    def search(self, query: str, k: int = 5, filters: Dict = None) -> List[Dict]:
        """Search the index."""
        query_tokens = self._tokenize(query)
        
        # Calculate scores
        scores = []
        for doc_idx, doc in enumerate(self.documents):
            # Apply filters
            if filters:
                if not self._match_filters(doc['metadata'], filters):
                    continue
            
            score = self._calculate_bm25(doc_idx, query_tokens)
            if score > 0:
                scores.append((score, doc_idx))
        
        # Sort by score
        scores.sort(reverse=True)
        
        # Return top k
        results = []
        for score, doc_idx in scores[:k]:
            doc = self.documents[doc_idx]
            results.append({
                'id': doc['id'],
                'text': doc['text'],
                'metadata': doc['metadata'],
                'score': score
            })
        
        return results
    
    def _match_filters(self, metadata: Dict, filters: Dict) -> bool:
        """Check if metadata matches filters."""
        for key, value in filters.items():
            if key not in metadata:
                return False
            if isinstance(value, list):
                if metadata[key] not in value:
                    return False
            elif metadata[key] != value:
                return False
        return True
    
    def save(self, path: str):
        """Save index to disk."""
        data = {
            'k1': self.k1,
            'b': self.b,
            'documents': self.documents,
            'doc_lengths': self.doc_lengths,
            'avg_doc_length': self.avg_doc_length,
            'doc_count': self.doc_count,
            'term_frequencies': dict(self.term_frequencies),
            'document_frequencies': dict(self.document_frequencies)
        }
        fs.write_json(path, data)
    
    def load(self, path: str):
        """Load index from disk."""
        data = fs.read_json(path)
        self.k1 = data['k1']
        self.b = data['b']
        self.documents = data['documents']
        self.doc_lengths = data['doc_lengths']
        self.avg_doc_length = data['avg_doc_length']
        self.doc_count = data['doc_count']
        self.term_frequencies = defaultdict(lambda: defaultdict(int))
        for term, docs in data['term_frequencies'].items():
            for doc_idx, count in docs.items():
                self.term_frequencies[term][int(doc_idx)] = count
        self.document_frequencies = defaultdict(int, data['document_frequencies'])
        self.idf_cache.clear()


def run(normalized_spec_path: str, config: Dict, out_dir: str) -> str:
    """
    Build RAG index from specifications and prompts.
    
    Args:
        normalized_spec_path: Path to normalized specification
        config: Project configuration
        out_dir: Output directory
        
    Returns:
        Path to index directory
    """
    # Create index directory
    index_dir = Path(out_dir) / 'rag_index'
    index_dir.mkdir(parents=True, exist_ok=True)
    
    # Load specification
    spec = fs.read_json(normalized_spec_path)
    
    # Create index
    index = SimpleIndex()
    
    # Index specification chunks
    index_specification(index, spec)
    
    # Index prompt templates
    index_prompts(index)
    
    # Index configuration
    index_configuration(index, config)
    
    # Index mapping types
    index_mappings(index)
    
    # Save index
    index_path = index_dir / 'index.json'
    index.save(str(index_path))
    
    # Save metadata
    metadata = {
        'created_at': datetime.utcnow().isoformat() + 'Z',
        'document_count': index.doc_count,
        'sources': {
            'specification': normalized_spec_path,
            'prompts': str(Path(__file__).parent.parent / 'prompts'),
            'config': 'config/project.yaml',
            'mappings': 'config/mapping_types.yaml'
        }
    }
    
    metadata_path = index_dir / 'metadata.json'
    fs.write_json(str(metadata_path), metadata)
    
    logger.info(f"Built RAG index with {index.doc_count} documents")
    
    return str(index_dir)


def index_specification(index: SimpleIndex, spec: Dict):
    """Index specification content."""
    # Index title and overview
    index.add_document(
        doc_id='spec_overview',
        text=f"Project: {spec.get('title', '')}. "
             f"COBOL Dialect: {spec.get('dialecte_cobol', '')}. "
             f"SQL Target: {spec.get('sql_cible', '')}",
        metadata={
            'source': 'specification',
            'type': 'overview',
            'layer': 'all'
        }
    )
    
    # Index functionalities
    for func in spec.get('fonctionnalites', []):
        index.add_document(
            doc_id=f"func_{func['id']}",
            text=f"{func['id']}: {func['resume']}",
            metadata={
                'source': 'specification',
                'type': 'functionality',
                'layer': 'business'
            }
        )
    
    # Index requirements
    for req in spec.get('exigences', []):
        layer = req.get('normalized', {}).get('layer', 'logic')
        index.add_document(
            doc_id=f"req_{req['id']}",
            text=f"{req['id']} ({req['type']}): {req['regle']}. "
                 f"Notes: {req.get('notes', '')}",
            metadata={
                'source': 'specification',
                'type': 'requirement',
                'req_type': req['type'],
                'layer': layer
            }
        )
    
    # Index entities
    for entity in spec.get('mcd', {}).get('entites', []):
        # Index entity overview
        attrs_text = ', '.join([f"{a['name']} ({a['type']})" for a in entity['attrs']])
        index.add_document(
            doc_id=f"entity_{entity['name']}",
            text=f"Entity {entity['name']} with attributes: {attrs_text}",
            metadata={
                'source': 'specification',
                'type': 'entity',
                'entity': entity['name'],
                'layer': 'dal'
            }
        )
        
        # Index each attribute
        for attr in entity['attrs']:
            attr_text = f"Attribute {attr['name']} of type {attr['type']}"
            if attr.get('pk'):
                attr_text += " (Primary Key)"
            if attr.get('fk'):
                attr_text += f" (Foreign Key to {attr['fk']})"
            if attr.get('nullable'):
                attr_text += " (Nullable)"
            if attr.get('default') is not None:
                attr_text += f" (Default: {attr['default']})"
            
            index.add_document(
                doc_id=f"attr_{entity['name']}_{attr['name']}",
                text=attr_text,
                metadata={
                    'source': 'specification',
                    'type': 'attribute',
                    'entity': entity['name'],
                    'attribute': attr['name'],
                    'layer': 'dal'
                }
            )
    
    # Index relations
    for rel in spec.get('mcd', {}).get('relations', []):
        index.add_document(
            doc_id=f"rel_{rel['from']}_{rel['to']}",
            text=f"Relationship from {rel['from']} to {rel['to']} "
                 f"({rel['type']}): {rel.get('label', '')}",
            metadata={
                'source': 'specification',
                'type': 'relation',
                'layer': 'dal'
            }
        )


def index_prompts(index: SimpleIndex):
    """Index prompt templates."""
    prompts_dir = Path(__file__).parent.parent / 'prompts'
    if not prompts_dir.exists():
        prompts_dir = Path('prompts')
    
    if prompts_dir.exists():
        for prompt_file in prompts_dir.glob('*.txt'):
            try:
                content = fs.read_text(str(prompt_file))
                
                # Determine layer from filename
                layer = 'all'
                if 'header' in prompt_file.name:
                    layer = 'all'
                elif 'procedure' in prompt_file.name:
                    layer = 'logic'
                elif 'sql' in prompt_file.name:
                    layer = 'dal'
                elif 'diagnosis' in prompt_file.name or 'repair' in prompt_file.name:
                    layer = 'autofix'
                
                index.add_document(
                    doc_id=f"prompt_{prompt_file.stem}",
                    text=content,
                    metadata={
                        'source': 'prompt',
                        'type': 'template',
                        'filename': prompt_file.name,
                        'layer': layer
                    }
                )
            except Exception as e:
                logger.warning(f"Failed to index prompt {prompt_file}: {e}")


def index_configuration(index: SimpleIndex, config: Dict):
    """Index project configuration."""
    # Index compiler configuration
    compiler = config.get('compiler', {})
    index.add_document(
        doc_id='config_compiler',
        text=f"Compiler: {compiler.get('name', 'cobc')}. "
             f"Flags: {compiler.get('flags', {}).get('base', '')}. "
             f"Line format: {compiler.get('line_format', 'free')}",
        metadata={
            'source': 'configuration',
            'type': 'compiler',
            'layer': 'all'
        }
    )
    
    # Index naming conventions
    naming = config.get('naming', {})
    index.add_document(
        doc_id='config_naming',
        text=f"Program naming: {naming.get('programs_case', 'UPPER-KEBAB')}. "
             f"Variable naming: {naming.get('variables_case', 'UPPER_SNAKE')}. "
             f"Working storage prefix: {naming.get('prefixes', {}).get('working_storage', 'WS-')}",
        metadata={
            'source': 'configuration',
            'type': 'naming',
            'layer': 'all'
        }
    )
    
    # Index LLM configuration
    llm = config.get('llm', {})
    index.add_document(
        doc_id='config_llm',
        text=f"LLM provider: {llm.get('provider', 'ollama')}. "
             f"Temperature: {llm.get('temperature', 0.7)}. "
             f"Max tokens: {llm.get('max_tokens', 4096)}",
        metadata={
            'source': 'configuration',
            'type': 'llm',
            'layer': 'all'
        }
    )


def index_mappings(index: SimpleIndex):
    """Index SQL to COBOL type mappings."""
    mapping_path = Path(__file__).parent.parent / 'config' / 'mapping_types.yaml'
    if not mapping_path.exists():
        mapping_path = Path('config/mapping_types.yaml')
    
    if mapping_path.exists():
        try:
            mappings = fs.read_yaml(str(mapping_path))
            
            for dialect, types in mappings.get('mappings', {}).items():
                for sql_type, cobol_mapping in types.items():
                    if isinstance(cobol_mapping, dict):
                        cobol_type = cobol_mapping.get('cobol', cobol_mapping.get('pattern', ''))
                        text = f"SQL {sql_type} maps to COBOL {cobol_type} in {dialect}"
                        
                        if 'range' in cobol_mapping:
                            text += f" (range: {cobol_mapping['range']['min']} to {cobol_mapping['range']['max']})"
                        
                        index.add_document(
                            doc_id=f"mapping_{dialect}_{sql_type}",
                            text=text,
                            metadata={
                                'source': 'mapping',
                                'type': 'type_mapping',
                                'dialect': dialect,
                                'sql_type': sql_type,
                                'layer': 'dal'
                            }
                        )
        except Exception as e:
            logger.warning(f"Failed to index mappings: {e}")


def index_files(paths: List[str]) -> Dict:
    """
    Index additional files.
    
    Args:
        paths: List of file paths to index
        
    Returns:
        Index statistics
    """
    index = SimpleIndex()
    indexed_count = 0
    
    for path in paths:
        try:
            if path.endswith('.json'):
                content = json.dumps(fs.read_json(path), indent=2)
            elif path.endswith('.yaml') or path.endswith('.yml'):
                content = str(fs.read_yaml(path))
            else:
                content = fs.read_text(path)
            
            index.add_document(
                doc_id=f"file_{Path(path).stem}",
                text=content,
                metadata={
                    'source': 'file',
                    'path': path,
                    'type': Path(path).suffix[1:] if Path(path).suffix else 'unknown'
                }
            )
            indexed_count += 1
            
        except Exception as e:
            logger.warning(f"Failed to index file {path}: {e}")
    
    return {
        'indexed': indexed_count,
        'total': len(paths),
        'index': index
    }


def search(query: str, k: int = 6, filters: Dict = None) -> List[Dict]:
    """
    Search the index.
    
    Args:
        query: Search query
        k: Number of results
        filters: Optional filters
        
    Returns:
        List of search results
    """
    # Find the latest index
    index_dirs = list(Path('out').glob('*/rag_index'))
    if not index_dirs:
        logger.warning("No RAG index found")
        return []
    
    latest_index = max(index_dirs, key=lambda p: p.stat().st_mtime)
    index_path = latest_index / 'index.json'
    
    if not index_path.exists():
        logger.warning(f"Index file not found: {index_path}")
        return []
    
    # Load and search
    index = SimpleIndex()
    index.load(str(index_path))
    
    return index.search(query, k, filters)