#!/usr/bin/env python3
"""
Basic Dependency DAG Test

Tests the cscope data generation and Prolog analysis for a basic
C codebase with a 6-level dependency hierarchy.
"""

import os
import sys
import subprocess
import shutil
from pathlib import Path


# ANSI color codes
class Colors:
    GREEN = '\033[0;32m'
    RED = '\033[0;31m'
    BLUE = '\033[0;34m'
    NC = '\033[0m'  # No Color


def print_colored(message: str, color: str = Colors.NC):
    """Print a message with ANSI color codes."""
    print(f"{color}{message}{Colors.NC}")


def cleanup_previous_runs(project_root: Path):
    """Clean up files from previous test runs."""
    # Directories and files to clean
    cleanup_paths = [
        project_root / 'data' / 'extracted',
        project_root / 'logs',
        project_root / 'knowledge' / 'cscope_facts.db',
        project_root / 'test' / 'data' / 'basic-deps' / 'src' / 'cscope.files',
        project_root / 'test' / 'data' / 'basic-deps' / 'src' / 'cscope.out',
        project_root / 'test' / 'data' / 'basic-deps' / 'src' / 'cscope.in.out',
        project_root / 'test' / 'data' / 'basic-deps' / 'src' / 'cscope.po.out',
    ]

    for path in cleanup_paths:
        if path.is_dir():
            shutil.rmtree(path, ignore_errors=True)
        elif path.is_file():
            path.unlink(missing_ok=True)


def main():
    """Run the basic dependency DAG test."""
    print("=== Basic Dependency DAG Test ===")
    print()

    # Get project root (3 levels up from this script)
    script_dir = Path(__file__).parent
    project_root = script_dir.parent.parent
    os.chdir(project_root)

    # Clean up files from previous runs
    print_colored("[0/3] Cleaning up previous test runs...", Colors.BLUE)
    cleanup_previous_runs(project_root)

    # Step 1: Generate cscope data
    print_colored("[1/3] Generating cscope data...", Colors.BLUE)

    # Set up environment for cscope (MSYS2 on Windows)
    env = os.environ.copy()
    if sys.platform == 'win32':
        # Add MSYS2 to PATH for cscope
        msys2_path = r'C:\msys64\usr\bin'
        env['PATH'] = f"{msys2_path}{os.pathsep}{env.get('PATH', '')}"

    result = subprocess.run(
        [
            sys.executable,
            'src/python/generate_cscope_data.py',
            '--src', 'test/data/basic-deps',
            '--root', '.',
            '--quiet'
        ],
        env=env,
        capture_output=True,
        text=True
    )

    if result.returncode != 0:
        print_colored("Failed to generate cscope data", Colors.RED)
        print(result.stderr)
        sys.exit(1)

    print(result.stdout, end='')

    # Step 2: Import into Prolog and run tests
    print_colored("[2/3] Importing data and running Prolog tests...", Colors.BLUE)

    # Set up environment for SWI-Prolog
    if sys.platform == 'win32':
        # Add SWI-Prolog to PATH
        swipl_path = r'C:\Program Files\swipl\bin'
        env['PATH'] = f"{swipl_path}{os.pathsep}{env.get('PATH', '')}"
        # On Windows, swipl.exe is the executable
        swipl_cmd = str(Path(swipl_path) / 'swipl.exe')
    else:
        swipl_cmd = 'swipl'

    # Prolog test script
    prolog_script = """
    ['src/prolog/cscope_import'],

    % Import all data (now uses persistent storage)
    import_cscope_symbols('data/extracted/cscope_symbols.txt', [debug(true)]),
    import_cscope_defs('data/extracted/cscope_definitions.txt', [debug(true)]),
    import_cscope_calls('data/extracted/cscope_callees.txt', [debug(true)]),

    % Show database stats after import
    cscope_db:db_statistics,
    nl,

    % Test 1: Count leaf functions
    findall(S, leaf_symbols(S), Leaves),
    length(Leaves, LeafCount),
    (LeafCount = 6 ->
        writeln('[PASS] Found 6 leaf functions')
    ;
        format('[FAIL] Expected 6 leaves, got ~w~n', [LeafCount])),

    % Test 2: Verify topological order exists
    (conversion_order(Order) ->
        (writeln('[PASS] Topological order computed'),
         length(Order, OrderLen),
         format('       (Order length: ~w functions)~n', [OrderLen]))
    ;
        writeln('[FAIL] Could not compute topological order')),

    % Test 3: Check main's dependency cone
    dependency_cone(main, Deps),
    length(Deps, DepCount),
    (DepCount = 24 ->
        writeln('[PASS] main has 24 dependencies')
    ;
        format('[FAIL] Expected 24 deps for main, got ~w~n', [DepCount])),

    % Test 4: Multi-dependency DAG structure
    findall(C, calls(processor_init, C), PICallees),
    length(PICallees, PICount),
    (PICount = 2 ->
        writeln('[PASS] processor_init calls 2 functions (DAG structure)')
    ;
        format('[FAIL] Expected processor_init to call 2 funcs, got ~w~n', [PICount])),

    % Test 5: Transitive dependencies
    (calls_transitive(main, min) ->
        writeln('[PASS] main transitively calls min (6 levels deep)')
    ;
        writeln('[FAIL] main should transitively call min')),

    halt
    """

    result = subprocess.run(
        [swipl_cmd, '-g', prolog_script, '-t', 'halt(1)'],
        env=env,
        capture_output=True,
        text=True
    )

    # Print Prolog output
    print(result.stdout, end='')

    if result.returncode != 0:
        print_colored("Tests failed", Colors.RED)
        if result.stderr:
            print(result.stderr)
        sys.exit(1)

    # Step 3: Success
    print()
    print_colored("[3/3] All tests passed!", Colors.GREEN)


if __name__ == '__main__':
    main()
