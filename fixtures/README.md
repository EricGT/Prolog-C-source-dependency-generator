# Test Fixtures

This directory contains test fixtures for validating the C dependency analysis tool.

## Test Cases

### `basic-deps/` - Basic Dependency DAG

A minimal C codebase with a 6-level dependency hierarchy structured as a directed acyclic graph (DAG).

**Directory Structure:**
```
basic-deps/
├── src/           # C source files (.c)
└── include/       # Header files (.h)
```

**Function Hierarchy:**
- **27 total functions** across 6 levels
- **Level 1**: 6 leaf functions (no dependencies)
  - `min()`, `max()`, `abs_val()` (math utilities)
  - `str_len()`, `str_equal()`, `str_copy()` (string utilities)
- **Level 2**: 4 functions calling Level 1
  - `buffer_init()`, `buffer_resize()` (buffer management)
  - `validate_length()`, `validate_content()` (validation)
- **Level 3**: 6 functions calling Level 2
  - `parse_init()`, `parse_token()`, `parse_string()` (parser)
  - `format_init()`, `format_output()`, `format_escape()` (formatter)
- **Level 4**: 6 functions calling Level 3
  - `processor_init()`, `processor_run()`, `processor_finalize()` (processor)
  - `analyzer_setup()`, `analyzer_check()`, `analyzer_report()` (analyzer)
- **Level 5**: 4 functions calling Level 4
  - `initialize()`, `process()`, `cleanup()`, `error_handler()` (application)
- **Level 6**: 1 top-level function calling Level 5
  - `main()` (entry point)

**Key Features:**
- **DAG structure**: Functions call multiple dependencies (not just a linear chain)
- **Realistic hierarchy**: Mimics actual C codebases with utilities → components → application
- **No cycles**: Pure acyclic structure for testing topological sort

**Testing Purpose:**
- Verify leaf detection works correctly (should find exactly 6 functions)
- Test topological sorting (leaf-first ordering)
- Validate DAG structure (multiple dependencies per function)
- Ensure transitive dependency tracking (main → ... → min is 6 levels deep)
- Confirm no false cycle detection

## Using Fixtures Manually

### Generate Cscope Data

The fixture source files are in the `src/` subdirectory:

```bash
# From project root - Unix/Mac
python src/python/generate_cscope_data.py --src fixtures/basic-deps/src --root .

# From project root - Windows (Git Bash)
python src/python/generate_cscope_data.py --src fixtures/basic-deps/src --root .

# From project root - Windows (native)
python src/python/generate_cscope_data.py --src "fixtures\basic-deps\src" --root .

# Using Prolog DCG extraction
swipl
?- ['src/prolog/cscope_extract'].
?- generate_cscope_data('fixtures/basic-deps/src', [root('.'), debug(1)]).
```

### Import and Query

```prolog
swipl
?- ['src/prolog/cscope_import'].

% First time - import data
?- import_cscope_symbols('data/extracted/cscope_symbols.txt', []).
?- import_cscope_defs('data/extracted/cscope_definitions.txt', []).
?- import_cscope_calls('data/extracted/cscope_callees.txt', []).

% Query the data
?- leaf_symbols(Symbol).              % Should find 6 leaf functions
Symbol = min ;
Symbol = max ;
Symbol = abs_val ;
Symbol = str_len ;
Symbol = str_equal ;
Symbol = str_copy.

?- dependency_cone(main, Deps).       % Find all dependencies of main
% Should return all 26 other functions

?- calls(processor_init, Callee).     % What does processor_init call?
% Should show multiple callees (DAG structure)
```

## Expected Data Counts

When running extraction on `basic-deps`:
- **Symbols**: ~108 (all symbol references in the codebase)
- **Definitions**: 27 (all function definitions)
- **Calls**: ~45 (all function call relationships)

## Adding New Fixtures

To add a new test fixture:

1. Create new directory: `fixtures/<test-name>/`
2. Add subdirectories: `src/` (C files) and `include/` (headers)
3. Add C source files with clear dependency structure
4. Document the fixture in this README
5. Create test script in `test/scripts/` if needed
6. Document expected results and test purpose
