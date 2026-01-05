#!/usr/bin/env python3
"""
COBOLSMARTGEN - Smart COBOL code generation with RAG and autofix
Main CLI entry point for orchestrating the pipeline.

Usage:
    python -m cobolsmartgen.cli.main run --input spec.yaml --dialecte gnucobol --sgbd postgres --out out
    python -m cobolsmartgen.cli.main generate --input spec.yaml --out out
    python -m cobolsmartgen.cli.main compile --out out
    python -m cobolsmartgen.cli.main fix --out out

    HWWBg6EZG571EblM7xl4ZFqXFhrQ7rB4
"""
import argparse
import shutil
import json
import logging
import os
import sys
from pathlib import Path
from typing import Dict, List, Optional

# Try to import python-dotenv, fallback silently if not available
try:
    from dotenv import load_dotenv
    DOTENV_AVAILABLE = True
except ImportError:
    DOTENV_AVAILABLE = False

# Import internal modules
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from cobolsmartgen.utils import fs, pipeline_tracer
from cobolsmartgen.ingest import validate_and_normalize
from cobolsmartgen.analyze import extract_io, plan_programs
from cobolsmartgen.rag import indexer
from cobolsmartgen.generate import cobol_headers, cobol_procedures, sql_files, assemble_layout


class CobolSmartGen:
    """Main orchestrator for the COBOL generation pipeline."""
    
    def __init__(self, args: argparse.Namespace):
        self.args = args
        self.config = self._load_config()
        self.logger = self._setup_logging()
        self.out_dir = Path(args.out).resolve()

        # Confirmation guard for reruns when artifacts already exist
        if self.args.command in ["run", "generate"] and not getattr(self.args, "force_rerun", False):
            if self._has_existing_generation():
                if not self._confirm_rerun():
                    self.logger.info("Generation aborted by user (artifacts already present).")
                    raise SystemExit(0)

        # Guard before LLM-heavy phases
        self.confirm_llm = not getattr(self.args, "auto_llm", False)
        self.single_pass = getattr(self.args, "single_pass", False)
        
    def _load_config(self) -> Dict:
        """Load configuration from project.yaml and environment variables."""
        # Load .env file if available
        if DOTENV_AVAILABLE:
            env_file = Path('.env')
            if env_file.exists():
                load_dotenv(env_file)
        
        # Load project configuration
        config_path = Path(__file__).parent.parent / 'config' / 'project.yaml'
        if not config_path.exists():
            config_path = Path('config/project.yaml')
        
        config = {}
        if config_path.exists():
            config = fs.read_yaml(str(config_path))
        
        # Override with CLI arguments
        if hasattr(self.args, 'dialecte') and self.args.dialecte:
            config.setdefault('defaults', {})['dialecte_cobol'] = self.args.dialecte
        if hasattr(self.args, 'sgbd') and self.args.sgbd:
            config.setdefault('defaults', {})['sql_cible'] = self.args.sgbd
        if hasattr(self.args, 'compiler') and self.args.compiler:
            config.setdefault('compiler', {})['name'] = self.args.compiler
            
        # Apply environment variables
        for key, value in os.environ.items():
            if key.startswith('COBOL_') or key.startswith('LLM_') or key.startswith('DB_'):
                section = key.split('_')[0].lower()
                var_name = '_'.join(key.split('_')[1:]).lower()
                config.setdefault(section, {})[var_name] = value
                
        return config
    
    def _setup_logging(self) -> logging.Logger:
        """Configure logging based on configuration."""
        log_level = self.config.get('logging', {}).get('level', 'INFO')
        log_format = self.config.get('logging', {}).get(
            'format', 
            '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        
        # Console handler
        console_handler = logging.StreamHandler()
        console_handler.setLevel(getattr(logging, log_level))
        console_handler.setFormatter(logging.Formatter(log_format))
        
        # File handler if configured
        handlers = [console_handler]
        log_file = self.config.get('logging', {}).get('file')
        if log_file:
            log_path = Path(log_file)
            log_path.parent.mkdir(parents=True, exist_ok=True)
            file_handler = logging.FileHandler(log_path)
            file_handler.setLevel(getattr(logging, log_level))
            file_handler.setFormatter(logging.Formatter(log_format))
            handlers.append(file_handler)
        
        # Configure root logger
        logging.basicConfig(level=getattr(logging, log_level), handlers=handlers)
        
        return logging.getLogger('cobolsmartgen')

    def _has_existing_generation(self) -> bool:
        """Detect if the output directory already contains generated artifacts."""
        markers = [
            self.out_dir / "program_plan.json",
            self.out_dir / "normalized_spec.json",
            self.out_dir / "io_map.json",
            self.out_dir / "dal" / "*.cbl",
            self.out_dir / "logic" / "*.cbl",
            self.out_dir / "business" / "*.cbl",
        ]
        for m in markers:
            if str(m).endswith("*.cbl"):
                if list(m.parent.glob(m.name)):
                    return True
            elif m.exists():
                return True
        return False

    def _confirm_rerun(self) -> bool:
        """Ask the user if they want to rerun generation when artifacts exist."""
        prompt = (
            f"\n⚠️  Des artefacts existent déjà dans {self.out_dir}.\n"
            "Relancer la génération (écrasera les fichiers) ? [y/N]: "
        )
        try:
            answer = input(prompt).strip().lower()
            return answer in ("y", "yes", "o", "oui")
        except EOFError:
            return False

    def _confirm_llm_phases(self) -> bool:
        """Ask before launching LLM-heavy phases (headers/procs/autofix)."""
        if not self.confirm_llm:
            return True
        prompt = (
            "\n⚠️  Lancement des phases LLM (headers/procs/autofix). Cela peut générer beaucoup d'appels.\n"
            "Continuer ? [y/N]: "
        )
        try:
            answer = input(prompt).strip().lower()
            return answer in ("y", "yes", "o", "oui")
        except EOFError:
            return False
        
    def run_pipeline(self) -> int:
        """Execute the full pipeline: ingest → analyze → rag → generate → compile → autofix."""
        # AJOUTÉ : Initialisation du tracer
        trace_dir = pipeline_tracer.init_trace(str(self.out_dir), os.getenv("CSG_RUN_ID"))
        self.logger.info(f"Pipeline trace: {trace_dir}")
        
        try:
            self.logger.info("Starting COBOL generation pipeline")
            
            # Step 1: Validate and normalize input specification
            step_dir = pipeline_tracer.start_step(1, "normalize")  # AJOUTÉ
            self.logger.info(f"Step 1/11: Validating and normalizing input spec: {self.args.input}")
            
            # AJOUTÉ : Sauvegarder l'input brut
            with open(self.args.input, "r") as f:
                raw_input = f.read()
            pipeline_tracer.save_input("raw", raw_input, step_dir)
            
            normalized_spec_path = validate_and_normalize.run(
                self.args.input, 
                self.config, 
                str(self.out_dir)
            )
            
            # AJOUTÉ : Sauvegarder l'output
            normalized_spec = fs.read_json(normalized_spec_path)
            pipeline_tracer.save_output("normalized", normalized_spec, step_dir)
            pipeline_tracer.end_step(success=True, extra_meta={
                "entities_count": len(normalized_spec.get("entities", []))
            })
            step_dir = pipeline_tracer.start_step(2, "io_mapping")  # AJOUTÉ
            self.logger.info("Step 2/11: Extracting IO mappings")
            
            pipeline_tracer.save_input("normalized", normalized_spec, step_dir)  # AJOUTÉ
            
            io_map_path = extract_io.run(
                normalized_spec_path,
                self.config,
                str(self.out_dir)
            )
            
            # AJOUTÉ
            io_map = fs.read_json(io_map_path)
            pipeline_tracer.save_output("io_map", io_map, step_dir)
            pipeline_tracer.end_step(success=True, extra_meta={
                "entities_count": len(io_map.get("entities", []))
            })

 # Step 3: Plan programs
            step_dir = pipeline_tracer.start_step(3, "program_plan")  # AJOUTÉ
            self.logger.info("Step 3/11: Planning programs")
            
            pipeline_tracer.save_input("io_map", io_map, step_dir)  # AJOUTÉ
            
            program_plan_path = plan_programs.run(
                normalized_spec_path,
                io_map_path,
                self.config,
                str(self.out_dir)
            )
            
            # AJOUTÉ
            program_plan = fs.read_json(program_plan_path)
            pipeline_tracer.save_output("program_plan", program_plan, step_dir)
            pipeline_tracer.end_step(success=True, extra_meta={
                "programs_count": len(program_plan.get("programs", []))
            })

            # Step 4: Generate Architecture Contract
            if os.getenv("CSG_USE_CONTRACT", "1") == "1":
                step_dir = pipeline_tracer.start_step(4, "contract")
                self.logger.info("Step 4/11: Generating architecture contract")

                from cobolsmartgen.core import contract_generator
                contract = contract_generator.run(str(self.out_dir), self.config)

                pipeline_tracer.save_output("contract", contract, step_dir)
                pipeline_tracer.end_step(success=True, extra_meta={
                    "programs_count": len(contract.get("layers", {}))
                })
                self.logger.info(f"   🔒 Contract generated and locked")

# Step 5: Build RAG index
            step_dir = pipeline_tracer.start_step(5, "rag_index")  # AJOUTÉ
            self.logger.info("Step 5/11: Building RAG index")
            
            rag_index_path = indexer.run(
                normalized_spec_path,
                self.config,
                str(self.out_dir)
            )
            
            # AJOUTÉ
            pipeline_tracer.save_output("index_content", {
                "index_path": str(rag_index_path) if rag_index_path else None
            }, step_dir)
            pipeline_tracer.end_step(success=True)
            
            # Step 6: Generate SQL files (d'abord SQL + structures)
            step_dir = pipeline_tracer.start_step(6, "sql")  # AJOUTÉ
            self.logger.info("Step 6/11: Generating SQL files (DDL/DML en premier)")
            
            sql_files_list = sql_files.run(
                normalized_spec_path=str(normalized_spec_path),
                io_map_path=str(io_map_path),
                config=self.config,
                out_dir=str(self.out_dir)
            )
            
            pipeline_tracer.end_step(success=True, extra_meta={
                "files_count": len(sql_files_list)
            })

            # Step 7: Prepare copybooks/structures (référence pour LLM)
            step_dir = pipeline_tracer.start_step(7, "structures")
            self.logger.info("Step 7/11: Preparing copybooks/structures from IO map")

            # Enregistrer le chemin io_map pour les modules aval
            self.config.setdefault("io_map_path", str(io_map_path))

            structure_files = assemble_layout.prepare_structures(
                io_map_path=str(io_map_path),
                out_dir=str(self.out_dir)
            )

            pipeline_tracer.end_step(success=True, extra_meta={
                "files_count": len(structure_files)
            })
            
            # Confirmation before LLM phases (headers/procs/autofix)
            if not self._confirm_llm_phases():
                self.logger.info("LLM phases skipped by user request. Stopping pipeline after SQL/structures.")
                pipeline_tracer.finalize_trace(success=True)
                return 0

            # Step 8: Generate COBOL headers
            step_dir = pipeline_tracer.start_step(8, "headers")  # AJOUTÉ
            self.logger.info("Step 8/11: Generating COBOL headers")
            
            pipeline_tracer.save_input("program_plan", program_plan, step_dir)  # AJOUTÉ
            
            header_files = cobol_headers.run(
                config=self.config,
                out_dir=str(self.out_dir)
            )
            
            # AJOUTÉ : Copier les fichiers générés
            static_dir = step_dir / "static_headers"
            static_dir.mkdir(exist_ok=True)
            for header_file in header_files:
                if Path(header_file).exists():
                    shutil.copy(header_file, static_dir / Path(header_file).name)
            
            pipeline_tracer.end_step(success=True, extra_meta={
                "files_count": len(header_files)
            })
            
            # Step 9: Generate COBOL procedures
            step_dir = pipeline_tracer.start_step(9, "procedures")  # AJOUTÉ
            self.logger.info("Step 9/11: Generating COBOL procedures")
            
            procedure_files = cobol_procedures.run(
                plan_path=str(program_plan_path),
                io_map_path=str(io_map_path),
                config=self.config,
                out_dir=str(self.out_dir)
            )
            
            pipeline_tracer.end_step(success=True, extra_meta={
                "files_count": len(procedure_files)
            })
            
            # Step 10: Assemble final layout
            step_dir = pipeline_tracer.start_step(10, "assembly")  # AJOUTÉ
            self.logger.info("Step 10/11: Assembling final layout")
            
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

            # If single-pass is requested, stop here (no validation/autofix/compile)
            if self.single_pass:
                self.logger.info("Single-pass generation requested; skipping validation/autofix/compile.")
                pipeline_tracer.finalize_trace(success=True)
                return 0

            # Step 8.5: Validation and Correction
            self.logger.info("Step 10.5/11: Validating and correcting generated code")
            try:
                from cobolsmartgen.validate import run_correction_pipeline

                # Exécuter le pipeline de correction
                correction_stats = run_correction_pipeline(
                    str(self.out_dir),
                    max_iterations=3
                )

                # Logger les résultats
                self.logger.info(f"✅ Validation complete:")
                self.logger.info(f"   - Initial errors: {correction_stats['errors_initial']}")
                self.logger.info(f"   - Final errors: {correction_stats['errors_final']}")
                self.logger.info(f"   - Fixed copybooks: {correction_stats['fixed_copybooks']}")
                self.logger.info(f"   - Fixed decimals: {correction_stats['fixed_decimals']}")
                self.logger.info(f"   - Fixed signatures: {correction_stats.get('fixed_signatures',0)}")
                self.logger.info(f"   - Fixed missing programs: {correction_stats.get('fixed_missing_programs',0)}")
                self.logger.info(f"   - Fixed return codes: {correction_stats.get('fixed_return_codes',0)}")
                self.logger.info(f"   - LLM validation fixes: {correction_stats.get('fixed_llm',0)}")
                self.logger.info(f"   - Compilation fixes (compiler): {correction_stats.get('compilation_fixes',0)}")
                self.logger.info(f"   - Compilation fixes (LLM paragraphs): {correction_stats.get('compilation_fixes_llm',0)}")
                self.logger.info(f"   - Formatted files: {correction_stats['formatted_files']}")

                # Avertir si des erreurs persistent
                if correction_stats['errors_final'] > 0:
                    self.logger.warning(f"⚠️  {correction_stats['errors_final']} errors remain. Check validation_report.md")

            except Exception as e:
                self.logger.warning(f"Validation pipeline failed (non-critical): {e}")
                import traceback
                self.logger.debug(traceback.format_exc())

            if self.args.command in ['run', 'compile']:
                # Step 9: Compile suite
                step_dir = pipeline_tracer.start_step(11, "compilation")  # AJOUTÉ
                self.logger.info("Step 11/11: Running compilation suite")
                
                try:
                    from cobolsmartgen.compile import run_compile_suite
                    compile_report_path = run_compile_suite.run(
                        program_plan_path,
                        self.config,
                        str(self.out_dir)
                    )
                except ImportError:
                    self.logger.error("compile module not available (was removed with --single-pass)")
                    pipeline_tracer.finalize_trace(success=False, error="compile module removed")
                    return 1
                
               # Check if compilation failed
                with open(compile_report_path, 'r') as f:
                    report = json.load(f)
                
                failed_count = sum(1 for r in report['results'] if r['rc'] != 0)
                
                # AJOUTÉ : Sauvegarder le rapport de compilation
                pipeline_tracer.save_output("compile_report", report, step_dir)
                pipeline_tracer.end_step(success=(failed_count == 0), extra_meta={
                    "failed_count": failed_count,
                    "total_count": len(report.get('results', []))
                })
                
                if failed_count > 0:
                    self.logger.warning(f"Compilation failed for {failed_count} files")
                    
                    if self.config.get('autofix', {}).get('enabled', True):
                        self.logger.info("Running autofix...")
                        success = self._run_autofix(compile_report_path)
                        if not success:
                            self.logger.error("Autofix exhausted without resolving all errors")
                            return 4
                    else:
                        return 3
            
            self.logger.info("Pipeline completed successfully")
            pipeline_tracer.finalize_trace(success=True)  # AJOUTÉ
            return 0
            
        except ValueError as e:
            self.logger.error(f"Validation error: {e}")
            pipeline_tracer.finalize_trace(success=False, error=str(e))  # AJOUTÉ
            return 2
        except Exception as e:
            self.logger.error(f"Pipeline failed: {e}")
            pipeline_tracer.finalize_trace(success=False, error=str(e))  # AJOUTÉ
            return 1
    
    def _run_autofix(self, compile_report_path: str) -> bool:
        """Run the autofix process with limited iterations."""
        try:
            from cobolsmartgen.autofix import diagnose, repair
        except ImportError:
            self.logger.error("autofix module not available (was removed with --single-pass)")
            return False
        
        max_iterations = self.config.get('autofix', {}).get('max_iterations', 2)
        
        for iteration in range(1, max_iterations + 1):
            self.logger.info(f"Autofix iteration {iteration}/{max_iterations}")
            
            # Diagnose errors
            diagnosis_paths = diagnose.run(
                compile_report_path,
                self.config,
                str(self.out_dir)
            )
            
            if not diagnosis_paths:
                self.logger.info("No errors to diagnose")
                return True
            
            # Apply repairs
            updated_files = repair.run(
                diagnosis_paths,
                self.config,
                str(self.out_dir)
            )
            
            if not updated_files:
                self.logger.info("No files were updated")
                return True
            
            # Recompile updated files
            self.logger.info(f"Recompiling {len(updated_files)} updated files")
            # Note: repair.py should update compile_report_path internally
            
            # Check if all errors are fixed
            with open(compile_report_path, 'r') as f:
                report = json.load(f)
            
            failed_count = sum(1 for r in report['results'] if r['rc'] != 0)
            
            if failed_count == 0:
                self.logger.info("All compilation errors resolved")
                return True
        
        return False
    
    def generate_only(self) -> int:
        """Execute generation phase only (steps 1-8)."""
        self.args.command = 'generate'
        return self.run_pipeline()
    
    def compile_only(self) -> int:
        """Execute compilation on existing generated files."""
        try:
            self.logger.info("Running compilation suite")
            
            # Find program plan
            program_plan_path = self.out_dir / 'program_plan.json'
            if not program_plan_path.exists():
                self.logger.error(f"Program plan not found: {program_plan_path}")
                return 1
            
            # Run compilation
            try:
                from cobolsmartgen.compile import run_compile_suite
                compile_report_path = run_compile_suite.run(
                    str(program_plan_path),
                    self.config,
                    str(self.out_dir)
                )
            except ImportError:
                self.logger.error("compile module not available (was removed with --single-pass)")
                return 1
            
            # Check results
            with open(compile_report_path, 'r') as f:
                report = json.load(f)
            
            failed_count = sum(1 for r in report['results'] if r['rc'] != 0)
            
            if failed_count > 0:
                self.logger.warning(f"Compilation failed for {failed_count} files")
                return 3
            
            self.logger.info("Compilation completed successfully")
            return 0
            
        except Exception as e:
            self.logger.error(f"Compilation failed: {e}")
            return 1
    
    def fix_only(self) -> int:
        """Run autofix on the last compilation report."""
        try:
            self.logger.info("Running autofix on last compilation report")
            
            # Find compile report
            compile_report_path = self.out_dir / 'reports' / 'compile.json'
            if not compile_report_path.exists():
                self.logger.error(f"Compilation report not found: {compile_report_path}")
                return 1
            
            success = self._run_autofix(str(compile_report_path))
            
            if success:
                self.logger.info("Autofix completed successfully")
                return 0
            else:
                self.logger.error("Autofix exhausted without resolving all errors")
                return 4
                
        except Exception as e:
            self.logger.error(f"Autofix failed: {e}")
            return 1


def main():
    """Main entry point for the CLI."""
    parser = argparse.ArgumentParser(
        description='COBOLSMARTGEN - Smart COBOL code generation with RAG and autofix',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    # Subcommands
    subparsers = parser.add_subparsers(dest='command', help='Commands')
    subparsers.default = 'run'
    
    # Common arguments
    common_parser = argparse.ArgumentParser(add_help=False)
    common_parser.add_argument(
        '--out',
        default='out',
        help='Output directory (default: out)'
    )
    common_parser.add_argument(
        '--config',
        help='Path to project.yaml configuration file'
    )
    common_parser.add_argument(
        '--log-level',
        choices=['DEBUG', 'INFO', 'WARNING', 'ERROR'],
        default='INFO',
        help='Logging level'
    )
    common_parser.add_argument(
        '--force-rerun',
        action='store_true',
        help='Skip confirmation when output directory already contains generated artifacts'
    )
    common_parser.add_argument(
        '--auto-llm',
        action='store_true',
        help='Skip confirmation before LLM phases (headers/procs/autofix)'
    )
    common_parser.add_argument(
        '--single-pass',
        action='store_true',
        help='Stop after generation (SQL, structures, headers, procedures, assembly); skip validation/autofix/compile and agents'
    )
    
    # Run command (full pipeline)
    run_parser = subparsers.add_parser(
        'run',
        parents=[common_parser],
        help='Run the full pipeline'
    )
    run_parser.add_argument(
        '--input',
        required=True,
        help='Input specification file (YAML/JSON)'
    )
    run_parser.add_argument(
        '--dialecte',
        choices=['gnucobol', 'ibm', 'microfocus'],
        help='COBOL dialect'
    )
    run_parser.add_argument(
        '--sgbd',
        choices=['postgres', 'mysql', 'oracle', 'sqlserver', 'db2', 'sqlite'],
        help='Target SQL database'
    )
    run_parser.add_argument(
        '--compiler',
        help='COBOL compiler to use'
    )
    
    # Generate command
    generate_parser = subparsers.add_parser(
        'generate',
        parents=[common_parser],
        help='Generate code without compilation'
    )
    generate_parser.add_argument(
        '--input',
        required=True,
        help='Input specification file (YAML/JSON)'
    )
    generate_parser.add_argument(
        '--dialecte',
        choices=['gnucobol', 'ibm', 'microfocus'],
        help='COBOL dialect'
    )
    generate_parser.add_argument(
        '--sgbd',
        choices=['postgres', 'mysql', 'oracle', 'sqlserver', 'db2', 'sqlite'],
        help='Target SQL database'
    )
    
    # Compile command
    compile_parser = subparsers.add_parser(
        'compile',
        parents=[common_parser],
        help='Compile existing generated files'
    )
    compile_parser.add_argument(
        '--compiler',
        help='COBOL compiler to use'
    )
    
    # Fix command
    fix_parser = subparsers.add_parser(
        'fix',
        parents=[common_parser],
        help='Run autofix on last compilation report'
    )
    
    args = parser.parse_args()
    
    # Default command
    if not args.command:
        args.command = 'run'
        if not hasattr(args, 'input'):
            parser.print_help()
            return 1
    
    # Create orchestrator
    app = CobolSmartGen(args)
    
    # Execute command
    if args.command == 'run':
        return app.run_pipeline()
    elif args.command == 'generate':
        return app.generate_only()
    elif args.command == 'compile':
        return app.compile_only()
    elif args.command == 'fix':
        return app.fix_only()
    else:
        parser.print_help()
        return 1


if __name__ == '__main__':
    sys.exit(main())
