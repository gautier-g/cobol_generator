# 
# # file: cobolsmartgen/generate/cobol_headers.py
# from __future__ import annotations
# import logging
# import os
# from pathlib import Path
# from typing import Dict, List
# 
# from ..utils import fs, trace
# 
# LOG = logging.getLogger(__name__)
# 
# _HEADER_TEMPLATE = """       IDENTIFICATION DIVISION.
#        PROGRAM-ID. {program_id}.
#        ENVIRONMENT DIVISION.
#        CONFIGURATION SECTION.
#        DATA DIVISION.
#        WORKING-STORAGE SECTION.
#        PROCEDURE DIVISION.
#            *> AUTO-GENERATED HEADER for layer {layer}
#            GOBACK.
# """
# 
# def _layer_folder(layer: str) -> str:
#     l = (layer or "logic").lower()
#     return l if l in ("dal", "logic", "business") else "logic"
# 
# def _generate_static_headers(config: Dict, out_dir: str) -> List[str]:
#     out = Path(out_dir)
#     plan_p = out / "program_plan.json"
#     if not plan_p.exists():
#         raise ValueError(f"Program plan not found: {plan_p}")
#     plan = fs.read_json(str(plan_p))
# 
#     created: List[str] = []
#     for item in plan.get("programs", []):
#         pid = item.get("id") or item.get("name") or "PROGRAM"
#         layer = item.get("layer") or "logic"
#         target_dir = fs.ensure_dir(out / _layer_folder(layer))
#         target = target_dir / f"{pid}.cbl"
# 
#         if not target.exists():
#             fs.write_text(str(target), _HEADER_TEMPLATE.format(program_id=pid, layer=layer), atomic=True)
#             trace.write_sidecar_hash(target)
#             trace.write_meta(target, kind="header", extra={"program_id": pid, "layer": layer})
# 
#         created.append(str(target))
# 
#     LOG.info("Static header files ready: %s", created)
#     return created
# 
# def _run(config: Dict, out_dir: str) -> List[str]:
#     created = _generate_static_headers(config, out_dir)
#     
#     use_llm = os.environ.get("CSG_USE_LLM_HEADERS", "1")
#     
#     if use_llm == "0":
#         LOG.info("LLM header generation disabled")
#         return created
#     
#     LOG.info("LLM header generation enabled, invoking mAInframer-34b...")
#     
#     try:
#         from . import _llm_codegen
#         llm_files = _llm_codegen.rewrite_headers_with_llm(out_dir, config)
#         LOG.info(f"LLM header generation successful: {len(llm_files)} files")
#         return llm_files
#     except Exception as e:
#         LOG.warning(f"LLM header generation failed, keeping static version: {e}")
#         return created
# 
# def run(*args, **kwargs):
#     from pathlib import Path as _P
#     config = kwargs.get("config")
#     out_dir = kwargs.get("out_dir")
# 
#     for a in args:
#         if isinstance(a, dict) and config is None:
#             config = a
#             break
# 
#     for a in reversed(args):
#         if isinstance(a, (str, _P)) and out_dir is None:
#             out_dir = str(a)
#             break
# 
#     if config is None:
#         config = {}
#     if out_dir is None:
#         out_dir = "out"
# 
#     return _run(config, out_dir)

"""
COBOL header generation module.
Creates static headers and coordinates LLM-based header generation.
"""

import logging
import os
from pathlib import Path
from typing import Dict, List

from ..utils import fs, trace

LOG = logging.getLogger(__name__)


def _layer_folder(layer: str) -> str:
    """Get folder name for layer."""
    l = (layer or "logic").lower()
    return l if l in ("dal", "logic", "business") else "logic"


def _generate_static_header(program_id: str, layer: str, entity: str) -> str:
    """Generate a static COBOL header matching the example format."""
    
    if layer == "dal":
        return f"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. {program_id}.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           *> AUTO-GENERATED HEADER for layer {layer}
           GOBACK.
"""

    
    elif layer == "logic":
        return f"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. {program_id}.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           *> AUTO-GENERATED HEADER for layer {layer}
           GOBACK.
"""

    
    else:  # business
        return f"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. {program_id}.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           *> AUTO-GENERATED HEADER for layer {layer}
           GOBACK.
"""



def generate_static_headers(config: Dict, out_dir: str) -> List[str]:
    """Generate static COBOL headers for all programs."""
    out = Path(out_dir)
    
    # Load program plan
    plan_path = out / "program_plan.json"
    if not plan_path.exists():
        raise ValueError(f"Program plan not found: {plan_path}")
    
    plan = fs.read_json(str(plan_path))
    touched = []
    
    for program in plan.get("programs", []):
        program_id = program.get("id") or program.get("name", "PROGRAM")
        layer = program.get("layer", "logic")
        entity = program.get("entity", "ENTITY")
        
        # Create target directory
        target_dir = out / _layer_folder(layer)
        fs.ensure_dir(target_dir)
        target_file = target_dir / f"{program_id}.cbl"
        
        # Generate header
        header = _generate_static_header(program_id, layer, entity)
        
        # Write file
        fs.write_text(str(target_file), header, atomic=True)
        trace.write_sidecar_hash(target_file)
        trace.write_meta(target_file, kind="header", extra={
            "program_id": program_id,
            "layer": layer,
            "entity": entity,
            "generated_by": "static"
        })
        
        touched.append(str(target_file))
        LOG.info(f"Generated static header: {target_file}")
    
    return touched


def run(config: Dict, out_dir: str) -> List[str]:
    """Main entry point for header generation."""
    LOG.info("Starting header generation")
    
    # Check if LLM generation is enabled
    use_llm = os.environ.get("CSG_USE_LLM_HEADERS", "0") == "1"
    
    if use_llm:
        LOG.info("LLM header generation enabled")
        try:
            from . import _llm_codegen
            return _llm_codegen.rewrite_headers_with_llm(out_dir, config)
        except Exception as e:
            LOG.warning(f"LLM generation failed, falling back to static: {e}")
            return generate_static_headers(config, out_dir)
    else:
        LOG.info("Using static header generation")
        return generate_static_headers(config, out_dir)
# def run(*args, **kwargs) -> List[str]:
#     """Compat: (config,out_dir) ou (norm_spec, plan, config, out_dir) ou (plan, out_dir)."""
#     if 'config' in kwargs or 'out_dir' in kwargs:
#         config = kwargs.get('config', {}) or {}
#         out_dir = str(kwargs.get('out_dir', 'out'))
#     elif len(args) == 4:
#         config = args[2] or {}
#         out_dir = str(args[3])
#     elif len(args) == 2:
#         config = {}
#         out_dir = str(args[1])
#     else:
#         config = {}
#         out_dir = 'out'
# 
#     LOG.info("Starting header generation")
#     use_llm = os.environ.get("CSG_USE_LLM_HEADERS", "0") == "1"
#     if use_llm:
#         LOG.info("LLM header generation enabled")
#         try:
#             from . import _llm_codegen
#             return _llm_codegen.rewrite_headers_with_llm(out_dir, config)
#         except Exception as e:
#             LOG.warning(f"LLM generation failed, falling back to static: {e}")
#             return generate_static_headers(config, out_dir)
#     else:
#         LOG.info("Using static header generation")
#         return generate_static_headers(config, out_dir)
# 

def run(*args, **kwargs) -> List[str]:
    """Compat: accepte (config,out_dir) ou (norm_spec, plan, config, out_dir) ou (plan, out_dir)."""
    # Résolution des paramètres (ne pas échouer si le pipeline passe 4 args)
    if 'config' in kwargs or 'out_dir' in kwargs:
        config = kwargs.get('config', {}) or {}
        out_dir = str(kwargs.get('out_dir', 'out'))
    elif len(args) == 4:
        # Appel du type: (program_plan_path, io_map_path, config, out_dir)
        config = args[2] or {}
        out_dir = str(args[3])
    elif len(args) == 2:
        # Appel du type: (config, out_dir) OU (plan_path, out_dir) -> on ne dépend que de out_dir
        config = args[0] if isinstance(args[0], dict) else {}
        out_dir = str(args[1])
    else:
        config = {}
        out_dir = 'out'

    LOG.info("Starting header generation")
    use_llm = os.environ.get("CSG_USE_LLM_HEADERS", "0") == "1"
    if use_llm:
        LOG.info("LLM header generation enabled")
        try:
            from . import _llm_codegen
            return _llm_codegen.rewrite_headers_with_llm(out_dir, config)
        except Exception as e:
            LOG.warning(f"LLM generation failed, falling back to static: {e}")
            return generate_static_headers(config, out_dir)
    else:
        LOG.info("Using static header generation")
        return generate_static_headers(config, out_dir)
