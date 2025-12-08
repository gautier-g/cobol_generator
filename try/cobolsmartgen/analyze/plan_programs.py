"""
Program planning module for generating 3-layer architecture programs.
Creates business, logic, and DAL layer programs based on requirements.
"""

import json
import logging
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional

from ..utils import fs, naming

logger = logging.getLogger(__name__)


def run(normalized_spec_path: str, io_map_path: str, config: Dict, out_dir: str) -> str:
    """
    Create program plan for 3-layer architecture.
    
    Args:
        normalized_spec_path: Path to normalized specification
        io_map_path: Path to IO map
        config: Project configuration
        out_dir: Output directory
        
    Returns:
        Path to program_plan.json
    """
    # Load inputs
    spec = fs.read_json(normalized_spec_path)
    io_map = fs.read_json(io_map_path)
    
    # Create program plan
    program_plan = create_program_plan(spec, io_map, config)
    
    # Add metadata
    program_plan['metadata'] = {
        'generated_at': datetime.utcnow().isoformat() + 'Z',
        'source_spec': normalized_spec_path,
        'source_io_map': io_map_path,
        'total_programs': len(program_plan['programs']),
        'layers': {
            'business': sum(1 for p in program_plan['programs'] if p['layer'] == 'business'),
            'logic': sum(1 for p in program_plan['programs'] if p['layer'] == 'logic'),
            'dal': sum(1 for p in program_plan['programs'] if p['layer'] == 'dal')
        }
    }
    
    # Write program plan
    output_path = Path(out_dir) / 'program_plan.json'
    output_path.parent.mkdir(parents=True, exist_ok=True)
    fs.write_json(str(output_path), program_plan, sort_keys=True)
    
    logger.info(f"Generated program plan with {len(program_plan['programs'])} programs")
    
    return str(output_path)


def create_program_plan(spec: Dict, io_map: Dict, config: Dict) -> Dict:
    """
    Create the complete program plan.
    
    Args:
        spec: Normalized specification
        io_map: IO mappings
        config: Project configuration
        
    Returns:
        Program plan dictionary
    """
    programs = []
    copybooks = []
    
    # Get naming configuration
    prog_case = spec.get('nommage', {}).get('programmes_case', 'UPPER-KEBAB')
    
    # Process each entity
    for entity_io in io_map['entities']:
        entity_name = entity_io['entity_name']
        
        # Create DAL program for this entity
        dal_program = create_dal_program(entity_name, entity_io, spec, prog_case)
        programs.append(dal_program)
        
        # Create Logic program for this entity
        logic_program = create_logic_program(entity_name, entity_io, spec, prog_case)
        programs.append(logic_program)
        
        # Create Business program for this entity
        business_program = create_business_program(entity_name, entity_io, spec, prog_case)
        programs.append(business_program)
        
        # Create entity copybook
        entity_copybook = create_entity_copybook(entity_name, entity_io, spec)
        copybooks.append(entity_copybook)
    
    # Add global copybooks
    for global_cb in io_map.get('global_copybooks', []):
        copybooks.append({
            'name': global_cb['name'],
            'type': 'constants' if 'STATUS' in global_cb['name'] else 'working',
            'path': f"copy/{global_cb['name']}.cpy",
            'fields': global_cb['fields'],
            'used_by': [p['id'] for p in programs]  # Used by all programs
        })
    
    # Create compilation sequence
    sequence = create_compilation_sequence(programs)
    
    return {
        'programs': programs,
        'copybooks': copybooks,
        'sequence': sequence
    }


def create_dal_program(entity_name: str, entity_io: Dict, spec: Dict, prog_case: str) -> Dict:
    """
    Create DAL layer program for an entity.
    
    Args:
        entity_name: Entity name
        entity_io: Entity IO mappings
        spec: Normalized specification
        prog_case: Program naming case
        
    Returns:
        DAL program definition
    """
    program_id = naming.program_id(entity_name, 'DAL', prog_case)
    
    procedures = []
    
    # Database connection procedures
    procedures.extend([
        {
            'name': 'OPEN-DATABASE',
            'type': 'connect',
            'description': 'Open database connection',
            'inputs': [],
            'outputs': ['WS-RETURN-CODE'],
            'calls': []
        },
        {
            'name': 'CLOSE-DATABASE',
            'type': 'disconnect',
            'description': 'Close database connection',
            'inputs': [],
            'outputs': ['WS-RETURN-CODE'],
            'calls': []
        }
    ])
    
    # CRUD procedures
    procedures.extend([
        {
            'name': f'SELECT-{entity_name.upper()}',
            'type': 'select',
            'description': f'Select {entity_name} by primary key',
            'inputs': [f for f in entity_io['inputs'] if any(a.get('pk') for a in get_entity_attrs(entity_name, spec))],
            'outputs': entity_io['outputs'],
            'calls': ['OPEN-DATABASE']
        },
        {
            'name': f'INSERT-{entity_name.upper()}',
            'type': 'insert',
            'description': f'Insert new {entity_name} record',
            'inputs': entity_io['inputs'],
            'outputs': ['WS-RETURN-CODE', 'SQLCODE'],
            'calls': ['OPEN-DATABASE']
        },
        {
            'name': f'UPDATE-{entity_name.upper()}',
            'type': 'update',
            'description': f'Update existing {entity_name} record',
            'inputs': entity_io['inputs'] + entity_io['outputs'],
            'outputs': ['WS-RETURN-CODE', 'SQLCODE'],
            'calls': ['OPEN-DATABASE']
        },
        {
            'name': f'DELETE-{entity_name.upper()}',
            'type': 'delete',
            'description': f'Delete {entity_name} record',
            'inputs': [f for f in entity_io['inputs'] if any(a.get('pk') for a in get_entity_attrs(entity_name, spec))],
            'outputs': ['WS-RETURN-CODE', 'SQLCODE'],
            'calls': ['OPEN-DATABASE']
        }
    ])
    
    # Add cursor operations if needed
    procedures.extend([
        {
            'name': f'OPEN-{entity_name.upper()}-CURSOR',
            'type': 'cursor_open',
            'description': f'Open cursor for {entity_name} records',
            'inputs': [],
            'outputs': ['WS-RETURN-CODE'],
            'calls': ['OPEN-DATABASE']
        },
        {
            'name': f'FETCH-{entity_name.upper()}-CURSOR',
            'type': 'cursor_fetch',
            'description': f'Fetch next {entity_name} record',
            'inputs': [],
            'outputs': entity_io['outputs'] + ['WS-EOF-FLAG'],
            'calls': []
        },
        {
            'name': f'CLOSE-{entity_name.upper()}-CURSOR',
            'type': 'cursor_close',
            'description': f'Close {entity_name} cursor',
            'inputs': [],
            'outputs': ['WS-RETURN-CODE'],
            'calls': []
        }
    ])
    
    return {
        'id': program_id,
        'layer': 'dal',
        'entity': entity_name,
        'purpose': f'Data Access Layer for {entity_name}',
        'file_path': f'dal/{program_id}.cbl',
        'procedures': procedures,
        'copybooks': [f'{entity_name.upper()}-RECORD', 'SQLCA', 'STATUS-CODES'],
        'dependencies': [],
        'deps': []
    }


def create_logic_program(entity_name: str, entity_io: Dict, spec: Dict, prog_case: str) -> Dict:
    """
    Create Logic layer program for an entity.
    
    Args:
        entity_name: Entity name
        entity_io: Entity IO mappings
        spec: Normalized specification
        prog_case: Program naming case
        
    Returns:
        Logic program definition
    """
    program_id = naming.program_id(entity_name, 'LOGIC', prog_case)
    dal_program_id = naming.program_id(entity_name, 'DAL', prog_case)
    
    procedures = []
    
    # Main orchestration procedures
    procedures.append({
        'name': f'PROCESS-{entity_name.upper()}',
        'type': 'orchestrate',
        'description': f'Main processing logic for {entity_name}',
        'inputs': entity_io['inputs'],
        'outputs': entity_io['outputs'],
        'calls': [f'READ-{entity_name.upper()}', f'VALIDATE-{entity_name.upper()}', 
                  f'CALCULATE-{entity_name.upper()}', f'WRITE-{entity_name.upper()}']
    })
    
    # Read procedure
    procedures.append({
        'name': f'READ-{entity_name.upper()}',
        'type': 'read',
        'description': f'Read {entity_name} data',
        'inputs': [f for f in entity_io['inputs'] if any(a.get('pk') for a in get_entity_attrs(entity_name, spec))],
        'outputs': entity_io['outputs'],
        'calls': [f'CALL "{dal_program_id}"']
    })
    
    # Validation procedure
    procedures.append({
        'name': f'VALIDATE-{entity_name.upper()}',
        'type': 'validate',
        'description': f'Validate {entity_name} data',
        'inputs': entity_io['inputs'],
        'outputs': ['WS-VALID-FLAG', 'WS-ERROR-MSG'],
        'calls': []
    })
    
    # Add calculation procedures based on requirements
    calc_procedures = extract_calc_procedures(entity_name, spec, entity_io)
    procedures.extend(calc_procedures)
    
    # Write procedure
    procedures.append({
        'name': f'WRITE-{entity_name.upper()}',
        'type': 'write',
        'description': f'Write {entity_name} data',
        'inputs': entity_io['inputs'] + entity_io['outputs'],
        'outputs': ['WS-RETURN-CODE'],
        'calls': [f'CALL "{dal_program_id}"']
    })
    
    # Add transformation procedures if needed
    procedures.append({
        'name': f'TRANSFORM-{entity_name.upper()}',
        'type': 'transform',
        'description': f'Transform {entity_name} data for output',
        'inputs': entity_io['outputs'],
        'outputs': entity_io['outputs'],
        'calls': []
    })
    
    return {
        'id': program_id,
        'layer': 'logic',
        'entity': entity_name,
        'purpose': f'Business Logic Layer for {entity_name}',
        'file_path': f'logic/{program_id}.cbl',
        'procedures': procedures,
        'copybooks': [f'{entity_name.upper()}-RECORD', 'STATUS-CODES'],
        'dependencies': [dal_program_id],
        'deps': [dal_program_id]
    }


def create_business_program(entity_name: str, entity_io: Dict, spec: Dict, prog_case: str) -> Dict:
    """
    Create Business/Presentation layer program for an entity.
    
    Args:
        entity_name: Entity name
        entity_io: Entity IO mappings
        spec: Normalized specification
        prog_case: Program naming case
        
    Returns:
        Business program definition
    """
    program_id = naming.program_id(entity_name, 'BUSINESS', prog_case)
    logic_program_id = naming.program_id(entity_name, 'LOGIC', prog_case)
    
    procedures = []
    
    # Main entry point
    procedures.append({
        'name': 'MAIN-PROCEDURE',
        'type': 'main',
        'description': 'Main entry point',
        'inputs': [],
        'outputs': [],
        'calls': [f'PRESENT-{entity_name.upper()}-MENU', f'PROCESS-{entity_name.upper()}-OPTION']
    })
    
    # Menu presentation
    procedures.append({
        'name': f'PRESENT-{entity_name.upper()}-MENU',
        'type': 'display',
        'description': f'Display {entity_name} menu options',
        'inputs': [],
        'outputs': ['WS-USER-OPTION'],
        'calls': []
    })
    
    # Option processing
    procedures.append({
        'name': f'PROCESS-{entity_name.upper()}-OPTION',
        'type': 'dispatch',
        'description': 'Process user selected option',
        'inputs': ['WS-USER-OPTION'],
        'outputs': [],
        'calls': [f'ADD-{entity_name.upper()}', f'VIEW-{entity_name.upper()}', 
                  f'UPDATE-{entity_name.upper()}', f'DELETE-{entity_name.upper()}',
                  f'GENERATE-{entity_name.upper()}-REPORT']
    })
    
    # CRUD operations at presentation layer
    procedures.extend([
        {
            'name': f'ADD-{entity_name.upper()}',
            'type': 'input',
            'description': f'Input new {entity_name} data',
            'inputs': [],
            'outputs': entity_io['inputs'],
            'calls': [f'CALL "{logic_program_id}"']
        },
        {
            'name': f'VIEW-{entity_name.upper()}',
            'type': 'display',
            'description': f'Display {entity_name} data',
            'inputs': entity_io['outputs'],
            'outputs': [],
            'calls': [f'CALL "{logic_program_id}"']
        },
        {
            'name': f'UPDATE-{entity_name.upper()}',
            'type': 'input',
            'description': f'Update {entity_name} data',
            'inputs': entity_io['inputs'],
            'outputs': entity_io['inputs'],
            'calls': [f'CALL "{logic_program_id}"']
        },
        {
            'name': f'DELETE-{entity_name.upper()}',
            'type': 'confirm',
            'description': f'Confirm {entity_name} deletion',
            'inputs': [],
            'outputs': ['WS-CONFIRM-FLAG'],
            'calls': [f'CALL "{logic_program_id}"']
        }
    ])
    
    # Report generation
    procedures.append({
        'name': f'GENERATE-{entity_name.upper()}-REPORT',
        'type': 'report',
        'description': f'Generate {entity_name} report',
        'inputs': [],
        'outputs': [],
        'calls': [f'CALL "{logic_program_id}"', f'FORMAT-{entity_name.upper()}-REPORT', 
                  f'PRINT-{entity_name.upper()}-REPORT']
    })
    
    procedures.extend([
        {
            'name': f'FORMAT-{entity_name.upper()}-REPORT',
            'type': 'format',
            'description': 'Format report data',
            'inputs': entity_io['outputs'],
            'outputs': ['WS-REPORT-LINE'],
            'calls': []
        },
        {
            'name': f'PRINT-{entity_name.upper()}-REPORT',
            'type': 'output',
            'description': 'Output report to file/screen',
            'inputs': ['WS-REPORT-LINE'],
            'outputs': [],
            'calls': []
        }
    ])
    
    return {
        'id': program_id,
        'layer': 'business',
        'entity': entity_name,
        'purpose': f'Business/Presentation Layer for {entity_name}',
        'file_path': f'business/{program_id}.cbl',
        'procedures': procedures,
        'copybooks': [f'{entity_name.upper()}-RECORD', 'STATUS-CODES'],
        'dependencies': [logic_program_id],
        'deps': [logic_program_id]
    }


def create_entity_copybook(entity_name: str, entity_io: Dict, spec: Dict) -> Dict:
    """
    Create copybook definition for an entity.
    
    Args:
        entity_name: Entity name
        entity_io: Entity IO mappings
        spec: Normalized specification
        
    Returns:
        Copybook definition
    """
    fields = []
    
    # Create record structure
    fields.append({
        'level': '01',
        'name': f'{entity_name.upper()}-RECORD',
        'pic': None
    })
    
    # Add all fields
    for field in entity_io['inputs'] + entity_io['outputs']:
        # Avoid duplicates
        if not any(f['name'] == field['name'] for f in fields):
            fields.append({
                'level': '05',
                'name': field['name'],
                'pic': field['cobol_type']
            })
    
    return {
        'name': f'{entity_name.upper()}-RECORD',
        'type': 'record',
        'path': f'copy/{entity_name.upper()}-RECORD.cpy',
        'fields': fields,
        'used_by': []  # Will be populated during dependency resolution
    }


def extract_calc_procedures(entity_name: str, spec: Dict, entity_io: Dict) -> List[Dict]:
    """
    Extract calculation procedures from requirements.
    
    Args:
        entity_name: Entity name
        spec: Normalized specification
        entity_io: Entity IO mappings
        
    Returns:
        List of calculation procedure definitions
    """
    procedures = []
    entity_upper = entity_name.upper()
    
    # Main calculation orchestrator
    procedures.append({
        'name': f'CALCULATE-{entity_upper}',
        'type': 'calculate',
        'description': f'Perform all calculations for {entity_name}',
        'inputs': entity_io['inputs'],
        'outputs': [f for f in entity_io['outputs'] if f.get('computed')],
        'calls': []
    })
    
    # Add specific calculation procedures from requirements
    for req in spec.get('exigences', []):
        if req['type'] == 'calcul':
            # Check if this calculation affects this entity
            if entity_upper in req['regle'].upper():
                calc_name = f'CALC-{req["id"]}'
                
                # Avoid duplicates
                if not any(p['name'] == calc_name for p in procedures):
                    procedures.append({
                        'name': calc_name,
                        'type': 'calculate',
                        'description': req.get('notes', req['regle'][:50]),
                        'inputs': extract_calc_inputs_from_rule(req['regle'], entity_io),
                        'outputs': extract_calc_outputs_from_rule(req['regle'], entity_io),
                        'calls': []
                    })
    
    return procedures


def extract_calc_inputs_from_rule(rule: str, entity_io: Dict) -> List[str]:
    """Extract calculation input fields from a rule."""
    import re
    inputs = []
    
    # Find all field references in the rule
    for field in entity_io['inputs']:
        if field['name'] in rule.upper():
            inputs.append(field['name'])
    
    return inputs


def extract_calc_outputs_from_rule(rule: str, entity_io: Dict) -> List[str]:
    """Extract calculation output fields from a rule."""
    import re
    outputs = []
    
    # Find assignment targets
    for field in entity_io['outputs']:
        if field.get('computed'):
            pattern = rf"{field['name']}\s*="
            if re.search(pattern, rule.upper()):
                outputs.append(field['name'])
    
    return outputs


def create_compilation_sequence(programs: List[Dict]) -> List[Dict]:
    """
    Create compilation sequence based on dependencies.
    
    Args:
        programs: List of program definitions
        
    Returns:
        Compilation sequence
    """
    sequence = []
    
    # Group by layer (DAL first, then Logic, then Business)
    dal_programs = [p['id'] for p in programs if p['layer'] == 'dal']
    logic_programs = [p['id'] for p in programs if p['layer'] == 'logic']
    business_programs = [p['id'] for p in programs if p['layer'] == 'business']
    
    # Stage 1: DAL programs (can be parallel)
    if dal_programs:
        sequence.append({
            'stage': 'dal',
            'order': 1,
            'parallel': True,
            'programs': dal_programs
        })
    
    # Stage 2: Logic programs (can be parallel within stage)
    if logic_programs:
        sequence.append({
            'stage': 'logic',
            'order': 2,
            'parallel': True,
            'programs': logic_programs
        })
    
    # Stage 3: Business programs (can be parallel within stage)
    if business_programs:
        sequence.append({
            'stage': 'business',
            'order': 3,
            'parallel': True,
            'programs': business_programs
        })
    
    return sequence


def get_entity_attrs(entity_name: str, spec: Dict) -> List[Dict]:
    """Get attributes for an entity from the specification."""
    for entity in spec.get('mcd', {}).get('entites', []):
        if entity['name'] == entity_name:
            return entity.get('attrs', [])
    return []