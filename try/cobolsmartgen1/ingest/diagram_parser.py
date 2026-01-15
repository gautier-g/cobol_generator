"""
Diagram parser module for extracting entities and relations from various diagram formats.
Supports Mermaid ER/ERD, PlantUML, and UML-JSON formats.
"""

import json
import logging
import re
from typing import Dict, List, Any, Optional

logger = logging.getLogger(__name__)


def parse_diagram(diagram: str) -> Dict:
    """
    Parse diagram from various formats.
    
    Args:
        diagram: Diagram string (Mermaid, PlantUML, UML-JSON, or URL)
        
    Returns:
        Dictionary with entities and relations
    """
    if not diagram:
        return {'entities': [], 'relations': []}
    
    # Check if it's a URL or file path
    if diagram.startswith(('http://', 'https://', 'file://')):
        logger.info(f"Diagram is a URL/path, skipping parse: {diagram}")
        return {'entities': [], 'relations': []}
    
    # Try to detect format
    diagram_trimmed = diagram.strip()
    
    # Check for JSON
    if diagram_trimmed.startswith('{'):
        return parse_uml_json(diagram_trimmed)
    
    # Check for Mermaid
    if 'erDiagram' in diagram_trimmed or 'graph' in diagram_trimmed:
        return parse_mermaid(diagram_trimmed)
    
    # Check for PlantUML
    if '@startuml' in diagram_trimmed or 'entity' in diagram_trimmed.lower():
        return parse_plantuml(diagram_trimmed)
    
    # Default to Mermaid attempt
    logger.warning("Could not detect diagram format, attempting Mermaid parse")
    return parse_mermaid(diagram_trimmed)


def parse_mermaid(diagram: str) -> Dict:
    """
    Parse Mermaid ER/ERD diagram.
    
    Args:
        diagram: Mermaid diagram string
        
    Returns:
        Dictionary with entities and relations
    """
    entities = []
    relations = []
    current_entity = None
    
    lines = diagram.strip().split('\n')
    
    for line in lines:
        line = line.strip()
        
        # Skip empty lines and comments
        if not line or line.startswith('%%'):
            continue
        
        # Entity definition start
        entity_match = re.match(r'^(\w+)\s*\{', line)
        if entity_match:
            current_entity = {
                'name': entity_match.group(1),
                'attrs': []
            }
            entities.append(current_entity)
            continue
        
        # Entity end
        if line == '}' and current_entity:
            current_entity = None
            continue
        
        # Attribute definition (inside entity)
        if current_entity:
            # Parse attribute: TYPE NAME [PK] [FK]
            attr_match = re.match(
                r'^\s*(\w+(?:\([^)]+\))?)\s+(\w+)\s*(PK|FK)?\s*(PK|FK)?',
                line
            )
            if attr_match:
                attr = {
                    'name': attr_match.group(2),
                    'type': attr_match.group(1)
                }
                
                # Check for PK/FK markers
                markers = [attr_match.group(3), attr_match.group(4)]
                if 'PK' in markers:
                    attr['pk'] = True
                if 'FK' in markers:
                    attr['fk'] = True
                
                current_entity['attrs'].append(attr)
                continue
        
        # Relationship definition
        rel_match = re.match(
            r'^(\w+)\s+([\|\}\{o\-]+)\s+([\|\}\{o\-]+)\s+(\w+)\s*:\s*(.+)?',
            line
        )
        if rel_match:
            from_entity = rel_match.group(1)
            to_entity = rel_match.group(4)
            label = rel_match.group(5) or ''
            
            # Determine cardinality from notation
            left_notation = rel_match.group(2)
            right_notation = rel_match.group(3)
            cardinality = determine_cardinality(left_notation, right_notation)
            
            relations.append({
                'from': from_entity,
                'to': to_entity,
                'type': cardinality,
                'label': label.strip()
            })
    
    return {
        'entities': entities,
        'relations': relations
    }


def parse_plantuml(diagram: str) -> Dict:
    """
    Parse PlantUML entity diagram.
    
    Args:
        diagram: PlantUML diagram string
        
    Returns:
        Dictionary with entities and relations
    """
    entities = []
    relations = []
    
    lines = diagram.strip().split('\n')
    current_entity = None
    
    for line in lines:
        line = line.strip()
        
        # Skip empty lines and directives
        if not line or line.startswith('@') or line.startswith('!'):
            continue
        
        # Entity definition
        entity_match = re.match(r'^entity\s+"?(\w+)"?\s*\{', line)
        if entity_match:
            current_entity = {
                'name': entity_match.group(1),
                'attrs': []
            }
            entities.append(current_entity)
            continue
        
        # Class definition (alternative syntax)
        class_match = re.match(r'^class\s+"?(\w+)"?\s*\{', line)
        if class_match:
            current_entity = {
                'name': class_match.group(1),
                'attrs': []
            }
            entities.append(current_entity)
            continue
        
        # End of entity
        if line == '}' and current_entity:
            current_entity = None
            continue
        
        # Attribute inside entity
        if current_entity:
            # Parse: [*] name : type
            attr_match = re.match(r'^\*?\s*(\w+)\s*:\s*(.+)', line)
            if attr_match:
                attr = {
                    'name': attr_match.group(1),
                    'type': attr_match.group(2).strip()
                }
                if line.startswith('*'):
                    attr['pk'] = True
                current_entity['attrs'].append(attr)
                continue
        
        # Relationship
        # Format: Entity1 ||--o{ Entity2 : label
        # Or: Entity1 -- Entity2
        rel_match = re.match(
            r'^"?(\w+)"?\s+([\|\}\{o\-\.\*]+)\s+"?(\w+)"?\s*:\s*(.+)?',
            line
        )
        if rel_match:
            relations.append({
                'from': rel_match.group(1),
                'to': rel_match.group(3),
                'type': '1:N',  # Default, would need more parsing for exact type
                'label': (rel_match.group(4) or '').strip()
            })
    
    return {
        'entities': entities,
        'relations': relations
    }


def parse_uml_json(diagram: str) -> Dict:
    """
    Parse UML-JSON format diagram.
    
    Args:
        diagram: JSON string representing UML diagram
        
    Returns:
        Dictionary with entities and relations
    """
    try:
        uml_data = json.loads(diagram)
        
        entities = []
        relations = []
        
        # Extract entities
        for entity_data in uml_data.get('entities', []):
            entity = {
                'name': entity_data['name'],
                'attrs': []
            }
            
            for attr_data in entity_data.get('attributes', []):
                attr = {
                    'name': attr_data['name'],
                    'type': attr_data.get('type', 'VARCHAR(255)')
                }
                
                if attr_data.get('primaryKey'):
                    attr['pk'] = True
                if attr_data.get('foreignKey'):
                    attr['fk'] = attr_data['foreignKey']
                if 'nullable' in attr_data:
                    attr['nullable'] = attr_data['nullable']
                if 'default' in attr_data:
                    attr['default'] = attr_data['default']
                
                entity['attrs'].append(attr)
            
            entities.append(entity)
        
        # Extract relations
        for rel_data in uml_data.get('relations', []):
            relations.append({
                'from': rel_data['from'],
                'to': rel_data['to'],
                'type': rel_data.get('cardinality', '1:N'),
                'label': rel_data.get('label', '')
            })
        
        return {
            'entities': entities,
            'relations': relations
        }
        
    except json.JSONDecodeError as e:
        logger.error(f"Failed to parse UML-JSON: {e}")
        return {'entities': [], 'relations': []}


def determine_cardinality(left: str, right: str) -> str:
    """
    Determine cardinality from Mermaid/PlantUML notation.
    
    Args:
        left: Left side notation
        right: Right side notation
        
    Returns:
        Cardinality string (1:1, 1:N, N:1, N:N)
    """
    # Simplified cardinality detection
    left_many = 'o{' in left or '}o' in left or '*' in left
    right_many = 'o{' in right or '}o' in right or '*' in right
    
    if left_many and right_many:
        return 'N:N'
    elif left_many:
        return 'N:1'
    elif right_many:
        return '1:N'
    else:
        return '1:1'


def integrate_into_spec(spec: Dict, parsed: Dict) -> Dict:
    """
    Integrate parsed diagram data into normalized specification.
    
    Args:
        spec: Normalized specification
        parsed: Parsed diagram data
        
    Returns:
        Updated specification with integrated diagram data
    """
    if not parsed or not parsed.get('entities'):
        logger.info("No diagram entities to integrate")
        return spec
    
    # Create entity name mapping
    spec_entities = {e['name']: e for e in spec.get('mcd', {}).get('entites', [])}
    
    # Integrate entities
    for parsed_entity in parsed['entities']:
        entity_name = parsed_entity['name']
        
        if entity_name in spec_entities:
            # Merge attributes
            existing_entity = spec_entities[entity_name]
            existing_attrs = {a['name']: a for a in existing_entity.get('attrs', [])}
            
            for parsed_attr in parsed_entity['attrs']:
                attr_name = parsed_attr['name']
                
                if attr_name not in existing_attrs:
                    # Add new attribute from diagram
                    new_attr = {
                        'name': attr_name,
                        'type': parsed_attr.get('type', 'VARCHAR(255)'),
                        'derived': True  # Mark as derived from diagram
                    }
                    
                    # Copy other properties
                    for key in ['pk', 'fk', 'nullable', 'default']:
                        if key in parsed_attr:
                            new_attr[key] = parsed_attr[key]
                    
                    existing_entity['attrs'].append(new_attr)
                    logger.info(f"Added attribute {attr_name} to entity {entity_name} from diagram")
                else:
                    # Merge properties if not already set
                    existing_attr = existing_attrs[attr_name]
                    for key in ['pk', 'fk']:
                        if key in parsed_attr and key not in existing_attr:
                            existing_attr[key] = parsed_attr[key]
        else:
            # Add new entity from diagram
            logger.warning(f"Entity {entity_name} found in diagram but not in spec, adding")
            new_entity = {
                'name': entity_name,
                'attrs': parsed_entity['attrs'],
                'derived': True  # Mark as derived from diagram
            }
            spec.setdefault('mcd', {}).setdefault('entites', []).append(new_entity)
    
    # Integrate relations
    if parsed.get('relations'):
        spec.setdefault('mcd', {}).setdefault('relations', []).extend(parsed['relations'])
    
    # Store parsed diagram metadata
    spec['diagramme'] = {
        'source': spec.get('diagramme', ''),
        'type': detect_diagram_type(spec.get('diagramme', '')),
        'parsed': True
    }
    
    return spec


def detect_diagram_type(diagram: str) -> str:
    """Detect the type of diagram format."""
    if not diagram:
        return 'unknown'
    
    diagram = diagram.strip()
    
    if diagram.startswith(('http://', 'https://', 'file://')):
        return 'url'
    elif diagram.startswith('{'):
        return 'uml-json'
    elif 'erDiagram' in diagram:
        return 'mermaid'
    elif '@startuml' in diagram:
        return 'plantuml'
    elif any(ext in diagram.lower() for ext in ['.png', '.jpg', '.jpeg', '.gif', '.svg']):
        return 'image'
    else:
        return 'mermaid'