#!/usr/bin/env python3
"""
Generate cscope data for C source dependency analysis.

This script extracts symbol-level dependencies from C source code using cscope,
producing line-oriented text files suitable for import into Prolog.

Directory Structure:
    The script creates the following directory structure in the project root:
    - data/extracted/: Extracted text data files
    - logs/: Log files (generation, file access, commands)

    Note: Cscope database files (cscope.out, etc.) remain in the source directory's
    src/ subdirectory because cscope expects them there for queries.

Modes:
    Interactive: Prompts for all inputs (no arguments)
    Command-line: Uses --src and --root arguments
    Help: Shows detailed usage information (-h/--help)

Examples:
    # Interactive mode (prompts for input)
    python src/python/generate_cscope_data.py

    # Command-line mode with explicit paths
    python src/python/generate_cscope_data.py --src /path/to/source --root .

    # With debug logging
    python src/python/generate_cscope_data.py --src /path/to/source --root . --debug 2

    # Dry run (show what would be done without executing)
    python src/python/generate_cscope_data.py --src /path/to/source --root . --dry-run

Logging:
    When debug level > 0, the script generates log files in logs/:
    - logs/cscope_generation.log: Timestamped operations and debug info
    - logs/files_accessed.txt: Complete list of C/H files discovered
    - logs/commands_run.txt: All cscope commands executed with results
"""

import argparse
import subprocess
import sys
import os
import shutil
import logging
from datetime import datetime
from pathlib import Path
from typing import Optional, Tuple, List, Dict, Set


class Logger:
    """Centralized logging for operations, commands, and file access."""

    def __init__(self, log_dir: Path, debug_level: int = 0):
        """
        Initialize logger.

        Args:
            log_dir: Directory for log files
            debug_level: 0=none, 1=basic, 2=verbose, 3=trace
        """
        self.log_dir = log_dir
        self.debug_level = debug_level
        self.start_time = datetime.now()

        # Track accessed files and executed commands
        self.accessed_files: Set[Path] = set()
        self.executed_commands: List[Dict] = []

        # Setup Python logging
        self.logger = logging.getLogger('cscope_generator')
        self.logger.setLevel(logging.DEBUG if debug_level > 0 else logging.INFO)

        # Create log file handler
        log_file = log_dir / 'cscope_generation.log'
        file_handler = logging.FileHandler(log_file, mode='w', encoding='utf-8')
        file_handler.setLevel(logging.DEBUG)

        # Format with timestamp
        formatter = logging.Formatter(
            '%(asctime)s [%(levelname)s] %(message)s',
            datefmt='%Y-%m-%d %H:%M:%S'
        )
        file_handler.setFormatter(formatter)
        self.logger.addHandler(file_handler)

        # Log session start
        self.logger.info('='*70)
        self.logger.info('Cscope Data Generation Session Started')
        self.logger.info(f'Debug Level: {debug_level}')
        self.logger.info('='*70)

    def log_operation(self, operation: str, details: str = ''):
        """Log a high-level operation."""
        msg = f"Operation: {operation}"
        if details:
            msg += f" - {details}"
        self.logger.info(msg)

    def log_command(self, command: List[str], cwd: Path, success: bool, output: str = ''):
        """Log an executed command."""
        cmd_str = ' '.join(command)
        timestamp = datetime.now().isoformat()

        self.executed_commands.append({
            'timestamp': timestamp,
            'command': cmd_str,
            'cwd': str(cwd),
            'success': success,
            'output': output[:200] if self.debug_level < 3 else output
        })

        self.logger.info(f"Command: {cmd_str}")
        self.logger.info(f"  Working Dir: {cwd}")
        self.logger.info(f"  Success: {success}")

        if self.debug_level >= 2:
            if output:
                self.logger.debug(f"  Output: {output[:500]}...")

        if self.debug_level >= 3:
            if output:
                self.logger.debug(f"  Full Output:\n{output}")

    def log_file_access(self, file_path: Path, access_type: str = 'read'):
        """Log file access."""
        self.accessed_files.add(file_path)

        if self.debug_level >= 2:
            self.logger.debug(f"File {access_type}: {file_path}")

    def log_debug(self, message: str, level: int = 1):
        """Log debug message at specified level."""
        if self.debug_level >= level:
            self.logger.debug(message)

    def log_error(self, message: str):
        """Log error message."""
        self.logger.error(message)

    def log_warning(self, message: str):
        """Log warning message."""
        self.logger.warning(message)

    def finalize(self):
        """Write summary files and close logs."""
        duration = datetime.now() - self.start_time

        self.logger.info('='*70)
        self.logger.info(f'Session Duration: {duration}')
        self.logger.info(f'Total Files Accessed: {len(self.accessed_files)}')
        self.logger.info(f'Total Commands Executed: {len(self.executed_commands)}')
        self.logger.info('='*70)

        # Write files_accessed.txt
        files_list_path = self.log_dir / 'files_accessed.txt'
        try:
            with open(files_list_path, 'w', encoding='utf-8') as f:
                f.write(f"# Files Accessed During Cscope Data Generation\n")
                f.write(f"# Generated: {datetime.now().isoformat()}\n")
                f.write(f"# Total Files: {len(self.accessed_files)}\n")
                f.write("#\n\n")

                for file_path in sorted(self.accessed_files):
                    f.write(f"{file_path}\n")

            self.logger.info(f"File inventory written: {files_list_path}")
        except Exception as e:
            self.logger.error(f"Failed to write file inventory: {e}")

        # Write commands_run.txt
        commands_path = self.log_dir / 'commands_run.txt'
        try:
            with open(commands_path, 'w', encoding='utf-8') as f:
                f.write(f"# Commands Executed During Cscope Data Generation\n")
                f.write(f"# Generated: {datetime.now().isoformat()}\n")
                f.write(f"# Total Commands: {len(self.executed_commands)}\n")
                f.write("#\n\n")

                for i, cmd in enumerate(self.executed_commands, 1):
                    f.write(f"[{i}] {cmd['timestamp']}\n")
                    f.write(f"Command: {cmd['command']}\n")
                    f.write(f"Working Directory: {cmd['cwd']}\n")
                    f.write(f"Success: {cmd['success']}\n")
                    if self.debug_level >= 2 and cmd['output']:
                        f.write(f"Output:\n{cmd['output']}\n")
                    f.write("\n" + "-"*70 + "\n\n")

            self.logger.info(f"Command log written: {commands_path}")
        except Exception as e:
            self.logger.error(f"Failed to write command log: {e}")


class Colors:
    """ANSI color codes for terminal output."""
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'

    @staticmethod
    def disable():
        """Disable colors (for non-TTY or when piping)."""
        Colors.HEADER = ''
        Colors.OKBLUE = ''
        Colors.OKCYAN = ''
        Colors.OKGREEN = ''
        Colors.WARNING = ''
        Colors.FAIL = ''
        Colors.ENDC = ''
        Colors.BOLD = ''


def print_header(msg: str, quiet: bool = False):
    """Print a section header."""
    if not quiet:
        print(f"\n{Colors.BOLD}{Colors.HEADER}=== {msg} ==={Colors.ENDC}", file=sys.stderr)


def print_success(msg: str, quiet: bool = False):
    """Print a success message."""
    if not quiet:
        print(f"{Colors.OKGREEN}[OK]{Colors.ENDC} {msg}", file=sys.stderr)


def print_info(msg: str, quiet: bool = False):
    """Print an info message."""
    if not quiet:
        print(f"{Colors.OKCYAN}[INFO]{Colors.ENDC} {msg}", file=sys.stderr)


def print_warning(msg: str):
    """Print a warning message (always shown)."""
    print(f"{Colors.WARNING}[WARN]{Colors.ENDC} {msg}", file=sys.stderr)


def print_error(msg: str):
    """Print an error message (always shown)."""
    print(f"{Colors.FAIL}[ERROR]{Colors.ENDC} {msg}", file=sys.stderr)


def check_cscope() -> bool:
    """Check if cscope is available in PATH."""
    return shutil.which('cscope') is not None


def find_msys64() -> Optional[Path]:
    """Find MSYS2 installation on Windows."""
    if sys.platform != 'win32':
        return None

    # Common MSYS2 installation locations
    common_paths = [
        Path('C:/msys64'),
        Path('C:/msys2'),
        Path.home() / 'msys64',
        Path.home() / 'msys2',
    ]

    for path in common_paths:
        if path.exists() and (path / 'usr' / 'bin' / 'bash.exe').exists():
            return path

    return None


def count_lines(file_path: Path) -> int:
    """Count lines in a file."""
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            return sum(1 for _ in f)
    except Exception:
        return 0

# TODO: Remove use of regular expressions and replace with Prolog DCGs.
# This could mean converting the code to Prolog entirely or calling Prolog as needed. 

def extract_symbol_from_context(context: str) -> Optional[tuple]:
    """
    Extract symbol name and kind from C code context line.

    Handles various C declaration patterns:
    - Functions: int foo(...) -> ('foo', 'function')
    - Structs: struct Foo { -> ('Foo', 'struct')
    - Typedefs: typedef ... Name; -> ('Name', 'typedef')
    - Enums: enum Bar { -> ('Bar', 'enum')
    - Macros: #define BAZ -> ('BAZ', 'macro')

    Filters out:
    - Simple variable declarations
    - Struct members
    - Local variables

    Returns:
        (symbol_name, kind) tuple or None if not a significant definition
    """
    import re

    context = context.strip()
    if not context:
        return None

    # Macro definition: #define SYMBOL
    if context.startswith('#define'):
        match = re.match(r'#define\s+([A-Za-z_][A-Za-z0-9_]*)', context)
        if match:
            return (match.group(1), 'macro')

    # Typedef: typedef ... NAME; or typedef struct NAME {
    if context.startswith('typedef'):
        # typedef struct/union/enum NAME {
        match = re.search(r'typedef\s+(?:struct|union|enum)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\{', context)
        if match:
            return (match.group(1), 'typedef')
        # typedef ... NAME;
        match = re.search(r'([A-Za-z_][A-Za-z0-9_]*)\s*;', context)
        if match:
            return (match.group(1), 'typedef')

    # Struct/union/enum: struct NAME {
    match = re.match(r'struct\s+([A-Za-z_][A-Za-z0-9_]*)\s*\{', context)
    if match:
        return (match.group(1), 'struct')
    match = re.match(r'union\s+([A-Za-z_][A-Za-z0-9_]*)\s*\{', context)
    if match:
        return (match.group(1), 'union')
    match = re.match(r'enum\s+([A-Za-z_][A-Za-z0-9_]*)\s*\{', context)
    if match:
        return (match.group(1), 'enum')

    # Function definition: Must have '(' and cannot be a simple single-char variable
    if '(' in context:
        # Look for pattern: TYPE FUNCNAME(
        # The function name comes right before the opening paren
        func_pattern = r'([A-Za-z_][A-Za-z0-9_]*)\s*\('
        matches = list(re.finditer(func_pattern, context))
        if matches:
            # Take the last match before '(' - that's likely the function name
            candidate = matches[-1].group(1)
            # Filter out C keywords and common type names
            keywords = {'if', 'while', 'for', 'switch', 'return', 'sizeof',
                       'int', 'char', 'void', 'short', 'long', 'float', 'double',
                       'unsigned', 'signed', 'const', 'static', 'extern', 'inline',
                       'struct', 'union', 'enum', 'typedef', 'volatile', 'register',
                       'auto', 'restrict'}
            if candidate.lower() not in keywords:
                return (candidate, 'function')

    # Skip simple variable declarations - these are noise
    # Pattern: ends with ; after single identifier
    # Example: "int x;" or "Type var;" or "} u;"
    if context.endswith(';'):
        # Check if line starts with } - it's a struct member
        if context.strip().startswith('}'):
            return None
        # Check if it's a simple variable (single short identifier before semicolon)
        simple_var = re.search(r'\s+([A-Za-z_][A-Za-z0-9_]*)\s*;$', context)
        if simple_var:
            var_name = simple_var.group(1)
            # If it's 1-2 letters, likely a struct member or local var
            if len(var_name) <= 2:
                return None

    # Fallback: return None - be selective about what we include
    return None


def normalize_path_for_prolog(filepath: str) -> str:
    """
    Normalize file path for Prolog:
    - Convert backslashes to forward slashes
    - Convert to lowercase

    This matches the normalization done by the Prolog parser.
    """
    # Replace backslashes with forward slashes
    normalized = filepath.replace('\\', '/')
    # Convert to lowercase
    normalized = normalized.lower()
    return normalized


def extract_definitions_from_symbols(symbols_file: Path, logger: Optional[Logger] = None) -> List[str]:
    """
    Extract function and type definitions from cscope symbols file.

    The symbols file has format: <filepath> <scope> <line> <context>
    Where scope is either "<global>" or a function name.

    For function definitions, we look for the first line where:
    - The scope is the function name itself (meaning it's inside that function)
    - The context contains '(' which indicates function definition/declaration

    For global definitions (structs, typedefs, macros), we look in <global> scope.

    Returns tab-separated format: <filepath>\t<symbol>\t<line> <context>
    with normalized paths (lowercase, forward slashes).
    """
    import re

    if not symbols_file.exists():
        if logger:
            logger.log_error(f"Symbols file not found: {symbols_file}")
        return []

    definitions = []
    seen_functions = set()

    try:
        with open(symbols_file, 'r', encoding='utf-8', errors='ignore') as f:
            for line in f:
                line = line.rstrip('\r\n')
                if not line:
                    continue

                # Parse: <filepath> <scope> <line> <context>
                parts = line.split(' ', 3)
                if len(parts) < 4:
                    continue

                filepath, scope, line_num, context = parts

                # Normalize filepath for Prolog
                filepath = normalize_path_for_prolog(filepath)

                # Handle function definitions
                if scope != '<global>' and scope not in seen_functions:
                    # This is the first line in a function's scope
                    # Check if context looks like a function definition
                    if '(' in context:
                        seen_functions.add(scope)
                        definitions.append(f"{filepath}\t{scope}\t{line_num} {context}")

                # Handle global definitions (structs, typedefs, enums, macros)
                elif scope == '<global>':
                    # Try to extract symbol from context
                    result = extract_symbol_from_context(context)
                    if result:
                        symbol, kind = result
                        # Only include non-function global definitions
                        if kind != 'function':
                            definitions.append(f"{filepath}\t{symbol}\t{line_num} {context}")

    except Exception as e:
        if logger:
            logger.log_error(f"Error reading symbols file: {e}")
        return []

    if logger:
        logger.log_operation(
            "Definitions extracted from symbols",
            f"{len(definitions)} definitions ({len(seen_functions)} functions)"
        )

    return definitions


def reformat_callee_line(line: str, caller: str) -> Optional[str]:
    """
    Reformat a cscope callee line to tab-separated format with normalized paths.

    Input format (space-separated): <filepath> <callee> <line> <context>
    Output format (tab-separated): <filepath>\t<caller>\t<line> <context>

    Args:
        line: Raw cscope output line
        caller: The function that is making the call

    Returns:
        Reformatted line or None if parsing fails
    """
    line = line.strip()
    if not line:
        return None

    # Parse: <filepath> <callee> <line> <context>
    parts = line.split(' ', 3)
    if len(parts) < 4:
        return None

    filepath, callee, line_num, context = parts

    # Normalize filepath for Prolog
    filepath = normalize_path_for_prolog(filepath)

    # Return tab-separated format with caller, not callee
    # Format: <filepath>\t<caller>\t<line> <context>
    return f"{filepath}\t{caller}\t{line_num} {context}"


def run_cscope_command(
    args: List[str],
    cwd: Path,
    output_file: Optional[Path] = None,
    verbose: bool = False,
    quiet: bool = False,
    logger: Optional[Logger] = None
) -> Tuple[bool, str]:
    """
    Run a cscope command.

    Returns:
        (success: bool, output: str)
    """
    try:
        if verbose:
            print_info(f"Running: cscope {' '.join(args)}", quiet)

        if logger:
            logger.log_debug(f"Executing cscope command: {' '.join(args)}", level=1)
            logger.log_debug(f"Working directory: {cwd}", level=2)

        result = subprocess.run(
            ['cscope'] + args,
            cwd=cwd,
            capture_output=True,
            text=True,
            encoding='utf-8',
            errors='replace'
        )

        success = result.returncode == 0
        output = result.stdout if success else result.stderr

        if logger:
            logger.log_command(['cscope'] + args, cwd, success, output)
            # Log output length for debugging zero-result issues
            if success:
                output_lines = len(output.splitlines()) if output else 0
                logger.log_debug(f"cscope returned {output_lines} lines", level=1)
                if output_lines == 0:
                    logger.log_warning("cscope query returned 0 results - database may be empty or pattern may not match")

        if not success:
            if logger:
                logger.log_error(f"cscope command failed: {result.stderr}")
            return False, result.stderr

        if output_file:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(result.stdout)

            if logger:
                logger.log_debug(f"Wrote output to: {output_file}", level=2)

        return True, result.stdout

    except FileNotFoundError:
        error_msg = "cscope command not found"
        if logger:
            logger.log_error(error_msg)
        return False, error_msg
    except Exception as e:
        error_msg = str(e)
        if logger:
            logger.log_error(f"Exception during cscope execution: {error_msg}")
        return False, error_msg


def discover_source_files(src_dir: Path, logger: Optional[Logger] = None) -> List[Path]:
    """
    Discover all C and H files in the source directory.

    Args:
        src_dir: Root source directory
        logger: Optional logger for tracking file access

    Returns:
        List of discovered source files
    """
    extensions = {'.c', '.h', '.cpp', '.hpp', '.cc', '.hh'}
    source_files = []

    if logger:
        logger.log_operation("Discovering source files", str(src_dir))

    try:
        for root, dirs, files in os.walk(src_dir):
            root_path = Path(root)
            for file in files:
                file_path = root_path / file
                if file_path.suffix.lower() in extensions:
                    source_files.append(file_path)
                    if logger:
                        logger.log_file_access(file_path, 'discovered')

        if logger:
            logger.log_operation(
                "Source file discovery complete",
                f"Found {len(source_files)} files"
            )

    except Exception as e:
        if logger:
            logger.log_error(f"Error discovering source files: {e}")
        raise

    return source_files


def prompt_path(prompt_text: str, default: Optional[str] = None, must_exist: bool = True) -> Path:
    """Prompt user for a path with validation."""
    while True:
        if default:
            user_input = input(f"{prompt_text} [{default}]: ").strip()
            path_str = user_input if user_input else default
        else:
            user_input = input(f"{prompt_text}: ").strip()
            if not user_input:
                print_error("Path cannot be empty")
                continue
            path_str = user_input

        path = Path(path_str).expanduser().resolve()

        if must_exist and not path.exists():
            print_error(f"Path does not exist: {path}")
            retry = input("Try again? (y/n): ").lower()
            if retry != 'y':
                sys.exit(1)
            continue

        return path


def interactive_mode() -> Tuple[Path, Path]:
    """
    Interactive mode: prompt user for all inputs.

    Returns:
        (src_dir, project_root)
    """
    print_header("Interactive Mode")
    print_info("This script will extract C source dependencies using cscope.")
    print_info("You will be prompted for the source directory and project root.\n")

    # Get source directory
    print(f"{Colors.BOLD}Source Directory{Colors.ENDC}")
    print("  This should be the root directory containing a 'src/' subdirectory")
    print("  with your C source files.\n")
    src_dir = prompt_path("Enter source directory", must_exist=True)

    # Validate src/ subdirectory exists
    src_subdir = src_dir / "src"
    if not src_subdir.exists():
        print_error(f"'{src_subdir}' not found")
        print_info("Expected structure: <source_dir>/src/*.c")
        sys.exit(1)

    # Get project root directory
    print(f"\n{Colors.BOLD}Project Root Directory{Colors.ENDC}")
    print("  Directory where data/, logs/, and knowledge/ subdirectories will be created.\n")
    project_root = prompt_path("Enter project root directory", default=".", must_exist=False)

    # Confirm
    print(f"\n{Colors.BOLD}Configuration:{Colors.ENDC}")
    print(f"  Source: {src_dir}")
    print(f"  Project Root: {project_root}")
    print(f"    Data will be in: {project_root / 'data'}")
    print(f"    Logs will be in: {project_root / 'logs'}")

    confirm = input("\nProceed? (y/n): ").lower()
    if confirm != 'y':
        print_info("Cancelled by user")
        sys.exit(0)

    return src_dir, project_root


def generate_cscope_data(
    src_dir: Path,
    project_root: Path,
    verbose: bool = False,
    quiet: bool = False,
    dry_run: bool = False,
    debug_level: int = 0
) -> bool:
    """
    Generate all cscope data files.

    Args:
        src_dir: Source directory (must contain src/ subdirectory)
        project_root: Project root directory (data/, logs/, knowledge/ subdirectories)
        verbose: Show verbose output
        quiet: Suppress all non-essential output
        dry_run: Show what would be done without executing
        debug_level: Debug level (0=none, 1=basic, 2=verbose, 3=trace)

    Returns:
        True if successful, False otherwise
    """
    # Define directory structure
    data_dir = project_root / 'data'
    extracted_dir = data_dir / 'extracted'
    log_dir = project_root / 'logs'

    # Create directories
    if not dry_run:
        data_dir.mkdir(parents=True, exist_ok=True)
        extracted_dir.mkdir(parents=True, exist_ok=True)
        log_dir.mkdir(parents=True, exist_ok=True)

    # Initialize logger
    logger = None
    if not dry_run:
        try:
            logger = Logger(log_dir, debug_level)
            logger.log_operation("Session started")
            logger.log_operation("Configuration", f"src={src_dir}, project_root={project_root}")
        except Exception as e:
            print_error(f"Failed to initialize logger: {e}")
            return False

    if not quiet:
        print_header("Cscope Data Generator")
        print_info(f"Source: {src_dir}")
        print_info(f"Project Root: {project_root}")
        print_info(f"  Data: {data_dir}")
        print_info(f"  Logs: {log_dir}")
        if debug_level > 0:
            print_info(f"Debug Level: {debug_level}")

    # Validate source directory
    if not src_dir.exists():
        error_msg = f"Source directory does not exist: {src_dir}"
        print_error(error_msg)
        if logger:
            logger.log_error(error_msg)
        return False

    src_subdir = src_dir / "src"
    if not src_subdir.exists():
        error_msg = f"'src' subdirectory not found: {src_subdir}"
        print_error(error_msg)
        print_info("Expected structure: <source_dir>/src/*.c")
        if logger:
            logger.log_error(error_msg)
        return False

    # Discover source files
    if not dry_run:
        try:
            source_files = discover_source_files(src_subdir, logger)
            if not quiet:
                print_info(f"Discovered {len(source_files)} source files")
        except Exception as e:
            error_msg = f"Failed to discover source files: {e}"
            print_error(error_msg)
            if logger:
                logger.log_error(error_msg)
            return False

    if dry_run:
        print_header("Dry Run - Operations that would be performed", quiet)

    # Step 1: Build cscope database
    print_header("Step 1/5: Building cscope database", quiet)

    if dry_run:
        print_info(f"Would run: cscope -b -k in {src_subdir} with file list", quiet)
    else:
        if logger:
            logger.log_operation("Building cscope database", f"cscope -b -k (in {src_subdir})")

        # Create cscope.files with just the source files we want to index
        # Use absolute paths to avoid cscope trying to resolve relative includes
        cscope_files = src_subdir / "cscope.files"
        files_written = 0
        try:
            with open(cscope_files, 'w', encoding='utf-8') as f:
                for src_file in source_files:
                    # Use absolute path
                    abs_path = src_file.resolve()
                    f.write(f"{abs_path}\n")
                    files_written += 1

                    # Log first few entries for debugging
                    if logger and files_written <= 3:
                        logger.log_debug(f"  cscope.files entry: {abs_path}", level=2)

            if logger:
                logger.log_debug(f"Created cscope.files at {cscope_files} with {files_written} entries", level=2)

            if not quiet:
                print_info(f"Created cscope.files: {cscope_files} ({files_written} files)")
        except Exception as e:
            error_msg = f"Failed to create cscope.files: {e}"
            print_error(error_msg)
            if logger:
                logger.log_error(error_msg)
            return False

        # Verify cscope.files was created and has content
        if not cscope_files.exists():
            error_msg = "cscope.files was not created"
            print_error(error_msg)
            if logger:
                logger.log_error(error_msg)
            return False

        cscope_files_size = cscope_files.stat().st_size
        if cscope_files_size == 0:
            error_msg = "cscope.files is empty - no files to index"
            print_error(error_msg)
            if logger:
                logger.log_error(error_msg)
            return False

        if logger:
            logger.log_debug(f"cscope.files size: {cscope_files_size} bytes", level=2)

        # Build the database from within the src/ subdirectory
        # Use -k flag to disable default include directory search
        # Use -b for build only (no interactive mode)
        # Use -q for fast symbol lookup (creates inverted index files)
        # cscope will read file list from cscope.files
        success, output = run_cscope_command(
            ['-b', '-k', '-q'],
            cwd=src_subdir,  # Run from src/ subdirectory
            verbose=verbose,
            quiet=quiet,
            logger=logger
        )

        if not success:
            error_msg = f"Failed to build cscope database: {output}"
            print_error(error_msg)
            if logger:
                logger.log_error(error_msg)
            return False

        cscope_out = src_subdir / "cscope.out"  # Database is now in src/
        if not cscope_out.exists():
            error_msg = "cscope.out was not created"
            print_error(error_msg)
            if logger:
                logger.log_error(error_msg)
            return False

        # Verify database is not empty
        db_size = cscope_out.stat().st_size
        if logger:
            logger.log_debug(f"cscope.out size: {db_size} bytes", level=2)

        if db_size == 0:
            error_msg = "cscope.out is empty - no symbols indexed"
            print_error(error_msg)
            if logger:
                logger.log_error(error_msg)
            return False

        print_success(f"Database created: {cscope_out} ({db_size:,} bytes)", quiet)
        if logger:
            logger.log_operation("Database created successfully", f"{cscope_out} ({db_size} bytes)")

        # Verify database can be queried (diagnostic test)
        if logger and debug_level > 0:
            logger.log_debug("Testing database query capability", level=1)
            test_success, test_output = run_cscope_command(
                ['-d', '-L', '-1', '.*'],  # -d = don't rebuild, use existing db
                cwd=src_subdir,  # Run from src/ subdirectory where database is
                verbose=False,
                quiet=True,
                logger=None  # Don't log this test query
            )
            if test_success:
                test_lines = len(test_output.splitlines()) if test_output else 0
                logger.log_debug(f"Test query returned {test_lines} results", level=1)
                if test_lines == 0:
                    logger.log_warning("Database test query returned 0 results - database may not contain indexed symbols")
            else:
                logger.log_warning(f"Database test query failed: {test_output}")

    # Step 2-5: Extract various types of data
    # Note: Using '.*' regex pattern instead of '*' to match all symbols
    # The '*' wildcard doesn't work as expected in cscope -L mode
    # Use -d flag to use existing database without rebuilding
    extractions = [
        {
            'step': 2,
            'name': 'symbols',
            'args': ['-d', '-L', '-0', '.*'],
            'output': 'cscope_symbols.txt',
            'description': 'Symbol references'
        },
        {
            'step': 3,
            'name': 'callers',
            'args': ['-d', '-L', '-3', '.*'],
            'output': 'cscope_callers.txt',
            'description': 'Reverse calls'
        },
        {
            'step': 4,
            'name': 'includes',
            'args': ['-d', '-L', '-8', '.*'],
            'output': 'cscope_includes.txt',
            'description': 'Include relationships'
        }
    ]

    stats = {}

    for extraction in extractions:
        print_header(f"Step {extraction['step']}/6: Extracting {extraction['name']}", quiet)
        output_file = extracted_dir / extraction['output']

        if dry_run:
            print_info(f"Would create: {output_file}", quiet)
            stats[extraction['name']] = 0
            continue

        if logger:
            logger.log_operation(
                f"Extracting {extraction['name']}",
                f"cscope {' '.join(extraction['args'])}"
            )

        # Extract directly to file
        success, output = run_cscope_command(
            extraction['args'],
            cwd=src_subdir,  # Run from src/ subdirectory where database is
            output_file=output_file,
            verbose=verbose,
            quiet=quiet,
            logger=logger
        )

        if not success:
            error_msg = f"Failed to extract {extraction['name']}: {output}"
            print_error(error_msg)
            if logger:
                logger.log_error(error_msg)
            return False

        line_count = count_lines(output_file)
        stats[extraction['name']] = line_count

        if logger:
            logger.log_operation(
                f"Extraction complete: {extraction['name']}",
                f"{line_count} records written to {output_file}"
            )

        # Warn if we got zero results
        if line_count == 0 and not quiet:
            print_warning(f"Zero {extraction['description']} found")
            if logger and debug_level == 0:
                print_info("Run with --debug 1 for more diagnostics")

        if quiet:
            # Machine-readable output for piping
            print(f"{extraction['output']}:{line_count}")
        else:
            print_success(f"Exported {line_count:,} {extraction['description']}", quiet)

    # Step 5: Extract definitions from symbols file
    # The -L -1 query doesn't work well, so we derive definitions from symbols
    print_header("Step 5/6: Extracting definitions from symbols", quiet)
    defs_output_file = extracted_dir / 'cscope_definitions.txt'

    if dry_run:
        print_info(f"Would create: {defs_output_file}", quiet)
        stats['definitions'] = 0
    else:
        if logger:
            logger.log_operation("Extracting definitions", "Processing symbols file")

        symbols_file = extracted_dir / 'cscope_symbols.txt'
        definitions = extract_definitions_from_symbols(symbols_file, logger)

        # Write definitions
        try:
            with open(defs_output_file, 'w', encoding='utf-8') as f:
                for line in definitions:
                    f.write(line + '\n')

            def_count = len(definitions)
            stats['definitions'] = def_count

            if def_count == 0 and not quiet:
                print_warning("Zero definitions extracted from symbols")
            else:
                print_success(f"Exported {def_count:,} definitions", quiet)

        except Exception as e:
            error_msg = f"Failed to write definitions file: {e}"
            print_error(error_msg)
            if logger:
                logger.log_error(error_msg)
            stats['definitions'] = 0

    # Step 6: Extract function calls by querying each function individually
    # The -L -2 query doesn't work with regex patterns, so we need to query each function
    print_header("Step 6/6: Extracting function calls", quiet)
    calls_output_file = extracted_dir / 'cscope_callees.txt'

    if dry_run:
        print_info(f"Would create: {calls_output_file}", quiet)
        stats['calls'] = 0
    else:
        if logger:
            logger.log_operation("Extracting function calls", "Querying each function individually")

        # Parse the symbols file to get all function names
        # In cscope symbol output, lines with function scope have format:
        # filename function_name linenum context
        # Lines in global scope have: filename <global> linenum context
        # So we extract all non-<global> entries from column 2
        symbols_file = extracted_dir / 'cscope_symbols.txt'
        function_names = set()

        if symbols_file.exists():
            try:
                with open(symbols_file, 'r', encoding='utf-8', errors='ignore') as f:
                    for line in f:
                        # Format: filename scope linenum context
                        # where scope is either "<global>" or a function name
                        parts = line.strip().split(maxsplit=3)
                        if len(parts) >= 4:
                            scope = parts[1]
                            # If the scope is not <global>, it's a function name
                            if scope != '<global>' and scope.isidentifier():
                                function_names.add(scope)

                if logger:
                    logger.log_debug(f"Found {len(function_names)} functions from symbols file", level=1)

            except Exception as e:
                error_msg = f"Failed to parse symbols file: {e}"
                print_error(error_msg)
                if logger:
                    logger.log_error(error_msg)

        # Query cscope for each function
        all_calls = []
        processed = 0
        failed = 0

        for func_name in function_names:
            processed += 1

            # Show progress for large sets
            if not quiet and processed % 100 == 0:
                print(f"  Processed {processed}/{len(function_names)} functions...", end='\r')

            if logger and debug_level >= 3:
                logger.log_debug(f"Querying calls for function: {func_name}", level=3)

            success, output = run_cscope_command(
                ['-d', '-L', '-2', func_name],
                cwd=src_subdir,
                verbose=False,
                quiet=True,
                logger=None  # Don't log every single query
            )

            if success and output:
                # Reformat each line to tab-separated format with caller
                lines = output.strip().split('\n')
                for line in lines:
                    if line.strip():
                        reformatted = reformat_callee_line(line, func_name)
                        if reformatted:
                            all_calls.append(reformatted)
            elif not success:
                failed += 1
                if logger and debug_level >= 2:
                    logger.log_debug(f"Failed to query function {func_name}: {output}", level=2)

        # Write all collected calls to output file
        try:
            with open(calls_output_file, 'w', encoding='utf-8') as f:
                for line in all_calls:
                    f.write(line + '\n')

            call_count = len(all_calls)
            stats['calls'] = call_count

            if logger:
                logger.log_operation(
                    "Function call extraction complete",
                    f"{call_count} calls from {processed} functions ({failed} queries failed)"
                )

            if not quiet:
                print(f"  Processed {processed}/{len(function_names)} functions" + " " * 20)  # Clear progress line
                if failed > 0:
                    print_warning(f"{failed} function queries failed")

            if call_count == 0 and not quiet:
                print_warning("Zero Function calls found")
                print_info("This may indicate:")
                print_info("  - No function calls in the code (unlikely)")
                print_info("  - Functions are not being correctly identified from definitions")
                print_info("  - cscope -L -2 query format issue")
                if debug_level == 0:
                    print_info("Run with --debug 2 for more diagnostics")
            else:
                print_success(f"Exported {call_count:,} Function calls", quiet)

        except Exception as e:
            error_msg = f"Failed to write calls file: {e}"
            print_error(error_msg)
            if logger:
                logger.log_error(error_msg)
            stats['calls'] = 0

    # Summary
    if not quiet and not dry_run:
        print_header("Summary")
        total = sum(stats.values())
        print(f"  Total records: {total:,}")
        for name, count in stats.items():
            print(f"    {name.capitalize():<15} {count:>10,}")

        # Diagnostic hints if we got zero results
        if total == 0:
            print_warning("\nZero records extracted - possible issues:")
            print_info("  1. Source directory might not contain C files in src/ subdirectory")
            print_info("  2. cscope database might not have indexed any symbols")
            print_info("  3. File permissions or encoding issues")
            print_info("\nTroubleshooting steps:")
            print_info(f"  - Check that {src_subdir} contains .c and .h files")
            print_info(f"  - Verify cscope.out size: ls -lh {src_subdir / 'cscope.out'}")
            print_info(f"  - Try manual query: cd {src_subdir} && cscope -L -1 '.*' | head")
            print_info(f"  - Run with --debug 3 for detailed diagnostics")

        if logger:
            logger.log_operation("Summary", f"Total records: {total}")

        print_header("Next Steps")
        print("\n1. Start SWI-Prolog:")
        print("   swipl\n")
        print("2. Load the cscope import module:")
        print("   ?- [cscope_import].\n")
        print("3. Import the data:")
        defs_path = str(extracted_dir / 'cscope_definitions.txt').replace('\\', '/')
        calls_path = str(extracted_dir / 'cscope_callees.txt').replace('\\', '/')
        print(f"   ?- import_cscope_defs('{defs_path}', user, [debug(true)]).")
        print(f"   ?- import_cscope_calls('{calls_path}', user, [debug(true)]).\n")
        print("4. Query the dependency graph:")
        print("   ?- leaf_symbols_in(user, Symbol).")
        print("   ?- conversion_order_in(user, Order).\n")

        print_success("Done!")

    # Finalize logger (write summary files)
    if logger:
        try:
            logger.finalize()
            if not quiet:
                print_info(f"Logs written to: {log_dir / 'cscope_generation.log'}")
                print_info(f"File inventory: {log_dir / 'files_accessed.txt'}")
                print_info(f"Commands log: {log_dir / 'commands_run.txt'}")
        except Exception as e:
            print_error(f"Failed to finalize logger: {e}")

    return True


def show_detailed_help():
    """Show detailed help information."""
    help_text = """
{bold}Cscope Data Generator for C Source Dependency Analysis{endc}

{header}DESCRIPTION{endc}
    This script uses cscope to extract symbol-level dependencies from C source
    code and produces line-oriented text files suitable for import into Prolog
    for dependency analysis and visualization.

{header}MODES OF OPERATION{endc}

    {cyan}Interactive Mode{endc}
        Run without arguments to be prompted for all inputs:
        $ python src/python/generate_cscope_data.py

    {cyan}Command-Line Mode{endc}
        Provide source directory and project root as arguments:
        $ python src/python/generate_cscope_data.py --src /path/to/source --root .

    {cyan}Quiet Mode (for piping){endc}
        Suppress all non-essential output, print machine-readable results:
        $ python src/python/generate_cscope_data.py --src /path/to/source --root . --quiet

    {cyan}Dry Run Mode{endc}
        Show what would be done without actually executing:
        $ python src/python/generate_cscope_data.py --src /path/to/source --root . --dry-run

    {cyan}Debug Mode{endc}
        Enable logging and debug output with levels (0=none, 1=basic, 2=verbose, 3=trace):
        $ python src/python/generate_cscope_data.py --src /path/to/source --root . --debug 2

{header}REQUIRED STRUCTURE{endc}
    The source directory must contain a 'src/' subdirectory with C source files:

        <source_dir>/
          src/
            file1.c
            file2.c
            file.h

{header}OUTPUT DIRECTORY STRUCTURE{endc}
    The script creates the following directory structure in the project root:

    {green}data/extracted/{endc}
        Extracted text data files:
        - cscope_definitions.txt: All global definitions (functions, structs, etc.)
        - cscope_callees.txt: Functions called by each function
        - cscope_callers.txt: Functions calling each function (reverse lookup)
        - cscope_symbols.txt: All symbol references throughout the codebase
        - cscope_includes.txt: File inclusion relationships (#include directives)

        Format: <file><TAB><symbol><TAB><line> <context>

    {green}logs/{endc}
        Log files (when --debug > 0):
        - cscope_generation.log: Timestamped operations and debug info
        - files_accessed.txt: Complete list of C/H files discovered
        - commands_run.txt: All cscope commands executed with results

    {green}knowledge/{endc}
        For Prolog knowledge bases and Cytoscape exports (created by user)

    {header}Note:{endc} Cscope database files (cscope.out, cscope.in.out, cscope.po.out)
    remain in the source directory's src/ subdirectory because cscope expects
    them there for queries.

{header}REQUIREMENTS{endc}
    - Python 3.7 or later
    - cscope installed and in PATH

    Installing cscope:
        Linux:   sudo apt install cscope
        macOS:   brew install cscope
        Windows: Install via MSYS2
                 "/c/msys64/usr/bin/bash" -lc "pacman -S --noconfirm msys/cscope"
                 (Script will detect MSYS2 location and provide exact command)

{header}EXAMPLES{endc}

    {cyan}Basic interactive usage:{endc}
        $ python src/python/generate_cscope_data.py
        [Prompts for source and project root directories]

    {cyan}Command-line with custom paths (Git Bash):{endc}
        $ python src/python/generate_cscope_data.py \\
            --src ~/Projects/sqlite-src-3510000 \\
            --root .

    {cyan}Quiet mode for scripting:{endc}
        $ python src/python/generate_cscope_data.py \\
            --src /path/to/source --root . --quiet
        cscope_definitions.txt:1523
        cscope_callees.txt:4892
        ...

    {cyan}Check what would happen without executing:{endc}
        $ python src/python/generate_cscope_data.py \\
            --src /path/to/source --root . --dry-run

    {cyan}Verbose debugging output:{endc}
        $ python src/python/generate_cscope_data.py \\
            --src /path/to/source --root . --verbose

    {cyan}Debug with comprehensive logging:{endc}
        $ python src/python/generate_cscope_data.py \\
            --src /path/to/source --root . --debug 2
        # Creates logs in logs/ directory

    {cyan}Maximum trace-level debugging:{endc}
        $ python src/python/generate_cscope_data.py \\
            --src /path/to/source --root . --debug 3
        # Includes full command output in logs

{header}INTEGRATION WITH PROLOG{endc}

    After generating the data files, use them in Prolog:

    1. Load the cscope import module:
       ?- ['src/prolog/cscope_import'].

    2. Import definitions:
       ?- import_cscope_defs('data/extracted/cscope_definitions.txt', user, [debug(true)]).

    3. Import function calls:
       ?- import_cscope_calls('data/extracted/cscope_callees.txt', user, [debug(true)]).

    4. Query dependencies:
       ?- leaf_symbols_in(user, Symbol).
       ?- calls_in(user, 'main', Callee).
       ?- dependency_cone_in(user, 'sqlite3VdbeExec', Deps).

    5. Export for visualization:
       ?- export_to_cytoscape(user, 'knowledge/vdbe_deps.json', [filter(vdbe)]).

{header}EXIT CODES{endc}
    0   Success
    1   Error (missing dependencies, invalid paths, etc.)
    130 Interrupted by user (Ctrl+C)

{header}AUTHOR{endc}
    Prolog C Source Dependency Generator Project
    For analyzing C source code dependencies

{header}SEE ALSO{endc}
    - docs/cscope-workflow.md - Complete workflow guide
    - src/prolog/cscope_import.pl - Prolog import module
    - viewer/index.html - Interactive visualization
""".format(
        bold=Colors.BOLD,
        endc=Colors.ENDC,
        header=Colors.HEADER,
        cyan=Colors.OKCYAN,
        green=Colors.OKGREEN
    )

    print(help_text)


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='Generate cscope data for C source dependency analysis',
        add_help=False,  # We'll handle help ourselves for detailed formatting
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    parser.add_argument(
        '--src',
        type=Path,
        help='Source directory (must contain src/ subdirectory)'
    )

    parser.add_argument(
        '--root',
        type=Path,
        help='Project root directory (will create data/, logs/, knowledge/ subdirectories)'
    )

    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Show verbose output'
    )

    parser.add_argument(
        '--quiet', '-q',
        action='store_true',
        help='Quiet mode (machine-readable output)'
    )

    parser.add_argument(
        '--dry-run', '-n',
        action='store_true',
        help='Show what would be done without executing'
    )

    parser.add_argument(
        '--no-color',
        action='store_true',
        help='Disable colored output'
    )

    parser.add_argument(
        '--debug', '-d',
        type=int,
        default=0,
        choices=[0, 1, 2, 3],
        help='Debug level: 0=none, 1=basic, 2=verbose, 3=trace (default: 0)'
    )

    parser.add_argument(
        '--help', '-h',
        action='store_true',
        help='Show detailed help'
    )

    args = parser.parse_args()

    # Handle help
    if args.help:
        show_detailed_help()
        return 0

    # Disable colors if needed
    if args.no_color or not sys.stderr.isatty() or args.quiet:
        Colors.disable()

    # Check for cscope
    if not check_cscope():
        print_error("cscope is not installed or not in PATH")
        print_info("Install cscope:")
        print_info("  Linux:   sudo apt install cscope")
        print_info("  macOS:   brew install cscope")

        # Windows-specific instructions
        if sys.platform == 'win32':
            msys64_path = find_msys64()
            if msys64_path:
                print_info(f"  Windows: MSYS2 found at {msys64_path}")
                bash_path = msys64_path / 'usr' / 'bin' / 'bash.exe'
                print_info(f'           Run: "{bash_path}" -lc "pacman -S --noconfirm msys/cscope"')
            else:
                print_error("  Windows: MSYS2/msys64 not found in common locations")
                print_info("           Common paths checked: C:/msys64, C:/msys2, ~/msys64, ~/msys2")
                print_info("           Install MSYS2 from: https://www.msys2.org/")
                print_info("           Then run: pacman -S --noconfirm msys/cscope")
        else:
            print_info("  Windows: Install MSYS2, then: pacman -S --noconfirm msys/cscope")

        return 1

    try:
        # Determine mode
        if args.src is None and args.root is None:
            # Interactive mode
            src_dir, project_root = interactive_mode()
        elif args.src is None or args.root is None:
            # Missing required arguments
            print_error("Both --src and --root are required in command-line mode")
            print_info("For interactive mode, run without arguments")
            print_info("For help, use: python generate_cscope_data.py --help")
            return 1
        else:
            # Command-line mode
            # Handle Windows absolute paths properly
            src_dir = Path(args.src).expanduser().resolve()
            project_root = Path(args.root).expanduser().resolve()

        # Generate data
        success = generate_cscope_data(
            src_dir=src_dir,
            project_root=project_root,
            verbose=args.verbose,
            quiet=args.quiet,
            dry_run=args.dry_run,
            debug_level=args.debug
        )

        return 0 if success else 1

    except KeyboardInterrupt:
        print_error("\nInterrupted by user")
        return 130
    except Exception as e:
        print_error(f"Unexpected error: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())
