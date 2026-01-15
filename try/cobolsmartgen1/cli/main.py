#!/usr/bin/env python3
"""
COBOLSMARTGEN - Smart COBOL code generation
Simplified CLI - Single mode operation only

Usage:
    python -m cobolsmartgen1.cli.main --input spec.yaml --out out_dir
"""
import argparse
import shutil
import json
import logging
import os
import sys
from pathlib import Path

# Hardcoded configuration - SINGLE MODE ONLY
os.environ['CSG_LLM_PROVIDER'] = 'mistral'
os.environ['CSG_USE_LLM_STEPS'] = '1'
os.environ['CSG_USE_LLM_HEADERS'] = '0'
os.environ['CSG_USE_LLM_PROCS'] = '1'
os.environ['CSG_LLM_PROGRAM_MODE'] = '1'
os.environ['CSG_STRICT_825'] = '1'
os.environ['CSG_USE_CONTRACT'] = '1'
os.environ['CSG_LLM_TIMEOUT_S'] = '600'

# Mistral API settings (override if already set in environment)
if 'MISTRAL_API_KEY' not in os.environ:
    os.environ['MISTRAL_API_KEY'] = 'VLThYvTaLkPjFZBAIHNor2lGYKhC4ubm'
    # 'HWWBg6EZG571EblM7xl4ZFqXFhrQ7rB4'
if 'MISTRAL_MODEL' not in os.environ:
    os.environ['MISTRAL_MODEL'] = 'mistral-large-latest'

# Import internal modules
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from cobolsmartgen1.utils import fs, pipeline_tracer
from cobolsmartgen1.ingest import validate_and_normalize
from cobolsmartgen1.analyze import extract_io, plan_programs
from cobolsmartgen1.rag_1 import indexer
from cobolsmartgen1.generate import cobol_headers, cobol_procedures, sql_files, assemble_layout, steps_generator


class CobolSmartGen:
    """Main orchestrator for the COBOL generation pipeline."""

    def __init__(self, input_file: str, out_dir: str):
        self.input_file = input_file
        self.out_dir = Path(out_dir).resolve()
        self.logger = self._setup_logging()

        # Hardcoded configuration
        self.config = {
            'defaults': {
                'dialecte_cobol': 'gnucobol',
                'sql_cible': 'postgres'
            },
            'logging': {
                'level': 'INFO'
            }
        }

        # Create output directory
        self.out_dir.mkdir(parents=True, exist_ok=True)

    def _setup_logging(self) -> logging.Logger:
        """Configure logging with INFO level."""
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s'
        )
        return logging.getLogger('cobolsmartgen')

    def run_pipeline(self) -> int:
        """Execute the full generation pipeline."""
        trace_dir = pipeline_tracer.init_trace(str(self.out_dir), os.getenv("CSG_RUN_ID"))
        self.logger.info(f"Pipeline trace: {trace_dir}")

        try:
            self.logger.info("Starting COBOL generation pipeline")

            # Step 1: Validate and normalize input specification
            step_dir = pipeline_tracer.start_step(1, "normalize")
            self.logger.info(f"Step 1/12: Validating and normalizing input spec: {self.input_file}")

            with open(self.input_file, "r") as f:
                raw_input = f.read()
            pipeline_tracer.save_input("raw", raw_input, step_dir)

            normalized_spec_path = validate_and_normalize.run(
                self.input_file,
                self.config,
                str(self.out_dir)
            )

            normalized_spec = fs.read_json(normalized_spec_path)
            pipeline_tracer.save_output("normalized", normalized_spec, step_dir)
            pipeline_tracer.end_step(success=True, extra_meta={
                "entities_count": len(normalized_spec.get("mcd", {}).get("entites", []))
                # len(normalized_spec.get("entities", []))
                
            })

            # Step 2: Extract IO mappings
            step_dir = pipeline_tracer.start_step(2, "io_mapping")
            self.logger.info("Step 2/12: Extracting IO mappings")

            pipeline_tracer.save_input("normalized", normalized_spec, step_dir)

            io_map_path = extract_io.run(
                normalized_spec_path,
                self.config,
                str(self.out_dir)
            )

            io_map = fs.read_json(io_map_path)
            pipeline_tracer.save_output("io_map", io_map, step_dir)
            pipeline_tracer.end_step(success=True, extra_meta={
                "entities_count": len(io_map.get("entities", []))
            })

            # Step 3: Plan programs
            step_dir = pipeline_tracer.start_step(3, "program_plan")
            self.logger.info("Step 3/12: Planning programs")

            pipeline_tracer.save_input("io_map", io_map, step_dir)

            program_plan_path = plan_programs.run(
                normalized_spec_path,
                io_map_path,
                self.config,
                str(self.out_dir)
            )

            program_plan = fs.read_json(program_plan_path)
            pipeline_tracer.save_output("program_plan", program_plan, step_dir)
            pipeline_tracer.end_step(success=True, extra_meta={
                "programs_count": len(program_plan.get("programs", []))
            })

            # Step 4: Generate Architecture Contract
            step_dir = pipeline_tracer.start_step(4, "contract")
            self.logger.info("Step 4/12: Generating architecture contract")

            from cobolsmartgen1.core import contract_generator
            contract = contract_generator.run(str(self.out_dir), self.config)

            pipeline_tracer.save_output("contract", contract, step_dir)
            pipeline_tracer.end_step(success=True, extra_meta={
                "programs_count": len(contract.get("layers", {}))
            })
            self.logger.info(f"    Contract generated and locked")

            # Step 5: Build RAG index
            step_dir = pipeline_tracer.start_step(5, "rag_index")
            self.logger.info("Step 5/12: Building RAG index")

            rag_index_path = indexer.run(
                normalized_spec_path,
                self.config,
                str(self.out_dir)
            )

            pipeline_tracer.save_output("index_content", {
                "index_path": str(rag_index_path) if rag_index_path else None
            }, step_dir)
            pipeline_tracer.end_step(success=True)

            # Step 6: Generate SQL files
            step_dir = pipeline_tracer.start_step(6, "sql")
            self.logger.info("Step 6/12: Generating SQL files (DDL/DML)")

            sql_files_list = sql_files.run(
                normalized_spec_path=str(normalized_spec_path),
                io_map_path=str(io_map_path),
                config=self.config,
                out_dir=str(self.out_dir)
            )

            pipeline_tracer.end_step(success=True, extra_meta={
                "files_count": len(sql_files_list)
            })

            # Step 7: Prepare copybooks/structures
            step_dir = pipeline_tracer.start_step(7, "structures")
            self.logger.info("Step 7/12: Preparing copybooks/structures from IO map")

            self.config.setdefault("io_map_path", str(io_map_path))

            structure_files = assemble_layout.prepare_structures(
                io_map_path=str(io_map_path),
                out_dir=str(self.out_dir)
            )

            pipeline_tracer.end_step(success=True, extra_meta={
                "files_count": len(structure_files)
            })

            # Step 8: Generate missing steps (LLM)
            step_dir = pipeline_tracer.start_step(8, "steps")
            self.logger.info("Step 8/12: Generating procedure steps (LLM)")

            steps_path = steps_generator.run(
                normalized_spec_path=str(normalized_spec_path),
                config=self.config,
                out_dir=str(self.out_dir),
                io_map_path=str(io_map_path),
            )
            pipeline_tracer.save_output("normalized_spec_steps", {
                "path": str(steps_path)
            }, step_dir)
            pipeline_tracer.end_step(success=True)

            # Step 9: Generate COBOL headers
            step_dir = pipeline_tracer.start_step(9, "headers")
            self.logger.info("Step 9/12: Generating COBOL headers")

            pipeline_tracer.save_input("program_plan", program_plan, step_dir)

            header_files = cobol_headers.run(
                config=self.config,
                out_dir=str(self.out_dir)
            )

            static_dir = step_dir / "static_headers"
            static_dir.mkdir(exist_ok=True)
            for header_file in header_files:
                if Path(header_file).exists():
                    shutil.copy(header_file, static_dir / Path(header_file).name)

            pipeline_tracer.end_step(success=True, extra_meta={
                "files_count": len(header_files)
            })

            # Step 10: Generate COBOL procedures
            step_dir = pipeline_tracer.start_step(10, "procedures")
            self.logger.info("Step 10/12: Generating COBOL procedures")

            procedure_files = cobol_procedures.run(
                plan_path=str(program_plan_path),
                io_map_path=str(io_map_path),
                config=self.config,
                out_dir=str(self.out_dir)
            )

            pipeline_tracer.end_step(success=True, extra_meta={
                "files_count": len(procedure_files)
            })

            # Step 11: Assemble final layout
            step_dir = pipeline_tracer.start_step(11, "assembly")
            self.logger.info("Step 11/12: Assembling final layout")

            generated_files = header_files + procedure_files + sql_files_list
            assemble_layout.run(
                str(program_plan_path),
                generated_files,
                self.config,
                str(self.out_dir)
            )

            pipeline_tracer.end_step(success=True, extra_meta={
                "total_files": len(generated_files)
            })

            # Single-pass mode: stop here (no validation/autofix/compile)
            self.logger.info("Generation complete (single-pass mode)")
            pipeline_tracer.finalize_trace(success=True)

            self.logger.info("Pipeline completed successfully")
            return 0

        except ValueError as e:
            self.logger.error(f"Validation error: {e}")
            pipeline_tracer.finalize_trace(success=False, error=str(e))
            return 2
        except Exception as e:
            self.logger.error(f"Pipeline failed: {e}")
            import traceback
            self.logger.error(traceback.format_exc())
            pipeline_tracer.finalize_trace(success=False, error=str(e))
            return 1


def main():
    """Main entry point for the CLI."""
    parser = argparse.ArgumentParser(
        description='COBOLSMARTGEN - Smart COBOL code generation (simplified single-mode)',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    parser.add_argument(
        '--input',
        required=True,
        help='Input specification file (YAML)'
    )
    parser.add_argument(
        '--out',
        required=True,
        help='Output directory'
    )

    args = parser.parse_args()

    # Create orchestrator
    app = CobolSmartGen(args.input, args.out)

    # Execute pipeline
    return app.run_pipeline()


if __name__ == '__main__':
    sys.exit(main())
