"""
Extract IO mappings from normalized specification.
Builds input/output maps per entity with SQL to COBOL type conversions.
"""

import json
import logging
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional

from ..utils import fs, typing_map, naming

logger = logging.getLogger(__name__)


def run(normalized_spec_path: str, config: Dict, out_dir: str) -> str:
    """
    Extract IO mappings from normalized specification.
    
    Args:
        normalized_spec_path: Path to normalized specification JSON
        config: Project configuration
        out_dir: Output directory
        
    Returns:
        Path to generated io_map.json file
    """
    # Load normalized specification
    spec = fs.read_json(normalized_spec_path)
    
    # Load mapping types configuration
    mapping_config_path = Path(__file__).parent.parent / 'config' / 'mapping_types.yaml'
    if not mapping_config_path.exists():
        mapping_config_path = Path('config/mapping_types.yaml')
    
    mapping_config = {}
    if mapping_config_path.exists():
        mapping_config = fs.read_yaml(str(mapping_config_path))
    
    # Get dialect from spec or config
    dialect = spec.get('dialecte_cobol') or config.get('defaults', {}).get('dialecte_cobol', 'gnucobol')
    
    # Build IO map
    io_map = build_io_map(spec, mapping_config, dialect)
    
    # Add metadata
    io_map['metadata'] = {
        'generated_at': datetime.utcnow().isoformat() + 'Z',
        'source_spec': normalized_spec_path,
        'dialect': dialect
    }
    
    # Write IO map
    output_path = Path(out_dir) / 'io_map.json'
    output_path.parent.mkdir(parents=True, exist_ok=True)
    fs.write_json(str(output_path), io_map, sort_keys=True)
    
    logger.info(f"Generated IO map: {output_path}")
    
    return str(output_path)


def build_io_map(spec: Dict, mapping_config: Dict, dialect: str) -> Dict:
    """
    Build IO map from specification and mapping configuration.
    
    Args:
        spec: Normalized specification
        mapping_config: SQL to COBOL type mappings
        dialect: COBOL dialect
        
    Returns:
        IO map dictionary
    """
    io_map = {
        'entities': [],
        'global_copybooks': []
    }
    
    # Process each entity
    for entity in spec.get('mcd', {}).get('entites', []):
        entity_io = {
            'name': entity['name'],  # Add 'name' for compatibility
            'entity_name': entity['name'],  # Keep for backward compatibility
            'inputs': [],
            'outputs': [],
            'procedures': []
        }
        
        # Process attributes
        for attr in entity.get('attrs', []):
            # Convert SQL type to COBOL
            sql_type = attr.get('normalized_type', {}).get('sql_type', attr['type'])
            cobol_type = typing_map.sql_to_cobol(
                sql_type,
                dialect,
                mapping_config.get('mappings', {}).get(dialect, {})
            )
            
            # Create IO field
            io_field = {
                'name': attr.get('normalized_name', attr['name']),
                'cobol_name': naming.to_cobol_name(attr.get('normalized_name', attr['name'])),
                'sql_type': sql_type,
                'cobol_type': cobol_type,
                'direction': determine_direction(attr, spec),
                'nullable': attr.get('nullable', True),
                'default': attr.get('default')
            }
            
            # Add validation rules if applicable
            validation = extract_validation_rules(attr, spec)
            if validation:
                io_field['validation'] = validation
            
            # Determine if input or output
            if io_field['direction'] in ['in', 'inout']:
                entity_io['inputs'].append(io_field)
            if io_field['direction'] in ['out', 'inout']:
                output_field = io_field.copy()
                
                # Check if computed field
                if is_computed_field(attr, spec):
                    output_field['computed'] = True
                    formula = extract_formula(attr, spec)
                    if formula:
                        output_field['formula'] = formula
                
                entity_io['outputs'].append(output_field)
        
        # Extract procedures for this entity
        entity_io['procedures'] = extract_entity_procedures(entity['name'], spec)
        
        io_map['entities'].append(entity_io)
    
    # Add global copybooks if SQL is used
    if needs_sql_copybooks(spec):
        io_map['global_copybooks'].append({
            'name': 'SQLCA',
            'purpose': 'SQL Communication Area',
            'fields': [
                {'name': 'SQLCODE', 'cobol_type': 'PIC S9(9) COMP-5'},
                {'name': 'SQLERRM', 'cobol_type': 'PIC X(70)'},
                {'name': 'SQLSTATE', 'cobol_type': 'PIC X(5)'}
            ]
        })
    
    # Add status/error handling copybook
    io_map['global_copybooks'].append({
        'name': 'STATUS-CODES',
        'purpose': 'Standard status and error codes',
        'fields': [
            {'name': 'WS-RETURN-CODE', 'cobol_type': 'PIC S9(4) COMP'},
            {'name': 'WS-ERROR-MSG', 'cobol_type': 'PIC X(255)'},
            {'name': 'WS-STATUS', 'cobol_type': 'PIC X'}
        ]
    })
    
    return io_map


def determine_direction(attr: Dict, spec: Dict) -> str:
    """
    Determine IO direction for an attribute.
    
    Args:
        attr: Attribute definition
        spec: Full specification
        
    Returns:
        Direction string: 'in', 'out', or 'inout'
    """
    # Primary keys are typically input for lookups
    if attr.get('pk'):
        return 'in'
    
    # Auto-generated fields are output only
    if attr.get('auto_generated'):
        return 'out'
    
    # Computed fields are output
    if is_computed_field(attr, spec):
        return 'out'
    
    # Foreign keys are input
    if attr.get('fk'):
        return 'in'
    
    # Default to inout for data fields
    return 'inout'


def is_computed_field(attr: Dict, spec: Dict) -> bool:
    """
    Check if an attribute is computed based on rules.
    
    Args:
        attr: Attribute definition
        spec: Full specification
        
    Returns:
        True if field is computed
    """
    attr_name = attr['name'].upper()
    
    # Check exigences for calculation rules
    for req in spec.get('exigences', []):
        if req['type'] in ['calcul', 'regle_metier']:
            rule = req['regle'].upper()
            if attr_name in rule:
                # Check if this field is on the left side of an assignment
                if f"{attr_name} =" in rule or f"{attr_name}=" in rule:
                    return True
    
    # Check for specific computed field patterns
    computed_patterns = ['_NET', '_TOTAL', '_SUM', '_AVG', '_COUNT', '_CALC']
    for pattern in computed_patterns:
        if pattern in attr_name:
            return True
    
    return False


def extract_formula(attr: Dict, spec: Dict) -> Optional[str]:
    """
    Extract calculation formula for a computed field.

    Args:
        attr: Attribute definition
        spec: Full specification

    Returns:
        Formula string or None
    """
    attr_name = attr['name'].upper()

    # Look for calculation rules
    for req in spec.get('exigences', []):
        if req['type'] in ['calcul', 'regle_metier']:
            rule = req['regle'].upper()
            if attr_name in rule:
                # Try to extract formula
                # Example: "SALARY_NET = ROUND(SALARY_BRUT * 0.7, 2)"
                import re
                # Fixed regex: don't stop at decimal points (0.7)
                # Only stop at sentence-ending period (followed by space or end)
                pattern = rf"{attr_name}\s*=\s*(.+?)(?:\s*\.\s*$|$)"
                match = re.search(pattern, rule)
                if match:
                    formula = match.group(1).strip()
                    # Remove trailing period if present
                    if formula.endswith('.'):
                        formula = formula[:-1].strip()
                    return formula

    return None


def extract_validation_rules(attr: Dict, spec: Dict) -> Optional[Dict]:
    """
    Extract validation rules for an attribute.
    
    Args:
        attr: Attribute definition
        spec: Full specification
        
    Returns:
        Validation rules dictionary or None
    """
    rules = {}
    attr_name = attr['name'].upper()
    
    # Check validation requirements
    for req in spec.get('exigences', []):
        if req['type'] == 'validation':
            rule = req['regle'].upper()
            if attr_name in rule:
                # Extract validation constraints
                import re
                
                # Check for range constraints
                range_match = re.search(rf"{attr_name}.*?(?:ENTRE|BETWEEN)\s*([\d.]+)\s*(?:ET|AND)\s*([\d.]+)", rule)
                if range_match:
                    rules['min'] = range_match.group(1)
                    rules['max'] = range_match.group(2)
                
                # Check for positive constraint
                if 'POSITIF' in rule or 'POSITIVE' in rule or '> 0' in rule or '>0' in rule:
                    rules['min'] = '0'
                
                # Check for max value
                max_match = re.search(rf"{attr_name}.*?(?:<|INFÉRIEUR|LESS).*?([\d.]+)", rule)
                if max_match:
                    rules['max'] = max_match.group(1)
    
    # Add type-based constraints
    sql_type = attr.get('normalized_type', {})
    if sql_type.get('length'):
        rules['max_length'] = sql_type['length']
    
    return rules if rules else None


def extract_entity_procedures(entity_name: str, spec: Dict) -> List[Dict]:
    """
    Extract procedures related to an entity.
    
    Args:
        entity_name: Name of the entity
        spec: Full specification
        
    Returns:
        List of procedure definitions
    """
    procedures = []
    entity_upper = entity_name.upper()
    
    # Standard CRUD operations
    procedures.extend([
        {
            'name': f'READ-{entity_upper}',
            'type': 'read',
            'inputs': [f'{entity_upper}-ID'],
            'outputs': [f'{entity_upper}-RECORD']
        },
        {
            'name': f'WRITE-{entity_upper}',
            'type': 'write',
            'inputs': [f'{entity_upper}-RECORD'],
            'outputs': ['WS-RETURN-CODE']
        },
        {
            'name': f'UPDATE-{entity_upper}',
            'type': 'update',
            'inputs': [f'{entity_upper}-RECORD'],
            'outputs': ['WS-RETURN-CODE']
        },
        {
            'name': f'DELETE-{entity_upper}',
            'type': 'delete',
            'inputs': [f'{entity_upper}-ID'],
            'outputs': ['WS-RETURN-CODE']
        }
    ])
    
    # Add calculation procedures based on requirements
    for req in spec.get('exigences', []):
        if req['type'] == 'calcul':
            # Check if this calculation affects this entity
            if entity_upper in req['regle'].upper():
                procedures.append({
                    'name': f'CALC-{req["id"]}',
                    'type': 'calculate',
                    'inputs': extract_calc_inputs(req['regle']),
                    'outputs': extract_calc_outputs(req['regle'])
                })
    
    # Add validation procedures
    for req in spec.get('exigences', []):
        if req['type'] == 'validation':
            if entity_upper in req['regle'].upper():
                procedures.append({
                    'name': f'VALIDATE-{entity_upper}',
                    'type': 'validate',
                    'inputs': [f'{entity_upper}-RECORD'],
                    'outputs': ['WS-VALID-FLAG', 'WS-ERROR-MSG']
                })
                break  # Only one validate procedure per entity
    
    return procedures


def extract_calc_inputs(rule: str) -> List[str]:
    """Extract input fields from a calculation rule."""
    import re
    # Simple extraction: find fields on right side of =
    match = re.search(r'=\s*(.+)', rule)
    if match:
        formula = match.group(1)
        # Extract variable names (uppercase words)
        variables = re.findall(r'\b[A-Z][A-Z0-9_]+\b', formula)
        return list(set(variables))
    return []


def extract_calc_outputs(rule: str) -> List[str]:
    """Extract output fields from a calculation rule."""
    import re
    # Find fields on left side of =
    match = re.search(r'(\b[A-Z][A-Z0-9_]+)\b\s*=', rule)
    if match:
        return [match.group(1)]
    return []


def needs_sql_copybooks(spec: Dict) -> bool:
    """Check if SQL copybooks are needed."""
    # Check if there's a SQL target
    if spec.get('sql_cible'):
        return True
    
    # Check for DAL requirements
    for req in spec.get('exigences', []):
        if req['type'] == 'dal':
            return True
    
    return False