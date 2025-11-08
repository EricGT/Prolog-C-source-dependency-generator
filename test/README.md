# Test Suite

This directory contains test fixtures and scripts for validating the C dependency analysis tool.

## Test Cases

### `data/basic-deps/` - Basic Dependency DAG

A minimal C codebase with a 6-level dependency hierarchy structured as a directed acyclic graph (DAG).

**Structure:**
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

**Purpose:**
- Verify leaf detection works correctly (should find exactly 6 functions)
- Test topological sorting (leaf-first ordering)
- Validate DAG structure (multiple dependencies per function)
- Ensure transitive dependency tracking (main → ... → min is 6 levels deep)
- Confirm no false cycle detection

## Running Tests

### Prerequisites

- Python 3.7+
- SWI-Prolog
- cscope (MSYS2 on Windows)

### Run Basic Deps Test

```bash
# From project root
python test/scripts/run_basic_deps_test.py
```

### Expected Output

```
=== Basic Dependency DAG Test ===

[0/3] Cleaning up previous test runs...
[1/3] Generating cscope data...
cscope_symbols.txt:51
cscope_callers.txt:0
cscope_includes.txt:26
[2/3] Importing data and running Prolog tests...
[PASS] Found 6 leaf functions
[PASS] Topological order computed
       (Order length: 67 functions)
[PASS] main has 24 dependencies
[PASS] processor_init calls 2 functions (DAG structure)
[PASS] main transitively calls min (6 levels deep)

[3/3] All tests passed!
```

## Manual Testing

You can also run queries manually:

```bash
# Generate data
python src/python/generate_cscope_data.py --src test/data/basic-deps --root .

# Start Prolog
swipl
```

```prolog
?- ['src/prolog/cscope_import'].
?- import_cscope_symbols('data/extracted/cscope_symbols.txt', user, []).
?- import_cscope_defs('data/extracted/cscope_definitions.txt', user, []).
?- import_cscope_calls('data/extracted/cscope_callees.txt', user, []).

% Find all leaf functions (should be 6)
?- findall(S, leaf_symbols_in(user, S), Leaves), length(Leaves, N).
N = 6,
Leaves = [min, max, abs_val, str_len, str_equal, str_copy].

% Get topological order (includes all symbols, may have duplicates)
?- conversion_order_in(user, Order), length(Order, Len).
Len = 67.
% Note: Order includes library functions (malloc, realloc, if) and may have duplicates

% Analyze main's dependencies (should be 24)
?- dependency_cone_in(user, main, Deps), length(Deps, N).
N = 24.
% Includes: 21 user functions + 3 library functions (malloc, realloc, if)

% Verify DAG structure - diamond dependency (multiple paths to same function)
?- findall(C, calls_in(user, processor_init, C), Callees).
Callees = [parse_init, format_init].
% Both parse_init and format_init call buffer_init (converging paths)
?- findall(C, calls_in(user, parse_init, C), ParseCallees).
ParseCallees = [buffer_init].
?- findall(C, calls_in(user, format_init, C), FormatCallees).
FormatCallees = [buffer_init].
% Diamond structure:
%   processor_init
%       ├── parse_init ──┐
%       └── format_init ─┴──> buffer_init

% Test transitive calls (6 levels deep)
?- calls_transitive_in(user, main, min).
true.
% Note: May return multiple solutions if there are multiple call paths

% Compute dependency levels (leaves = level 1)
?- compute_dependency_levels(user, Levels).
Levels = [1-[abs_val,max,min,str_copy,str_equal,str_len],
          2-[buffer_init,buffer_resize,format_escape,parse_string,validate_length],
          3-[analyzer_check,format_init,format_output,parse_init,parse_token,processor_finalize,validate_content],
          4-[analyzer_report,analyzer_setup,error_handler,processor_init,processor_run],
          5-[cleanup,initialize,process],
          6-[main]].

% Print a specific level
?- print_dependency_level(user, 1).
Level 1 (Leaves):
  [abs_val,max,min,str_copy,str_equal,str_len]

?- print_dependency_level(user, 2).
Level 2:
  buffer_init -> max, malloc
  buffer_resize -> abs_val, min, realloc
  format_escape -> if, if
  parse_string -> if, str_copy
  validate_length -> min, str_len

?- print_dependency_level(user, 6).
Level 6:
  main -> cleanup, initialize, process

% Print all levels (from top to bottom)
?- print_all_dependency_levels(user).
Level 6:
  main -> cleanup, initialize, process

Level 5:
  cleanup -> processor_finalize, analyzer_report
  initialize -> processor_init, analyzer_setup
  process -> processor_run, analyzer_check

Level 4:
  analyzer_report -> format_output, format_escape
  analyzer_setup -> parse_init, format_init
  error_handler -> format_init, format_escape, validate_content
  processor_init -> parse_init, format_init
  processor_run -> parse_token, format_output

Level 3:
  analyzer_check -> if, parse_string
  format_init -> buffer_init
  format_output -> if, buffer_resize
  parse_init -> buffer_init
  parse_token -> if, buffer_resize
  processor_finalize -> parse_string, format_escape
  validate_content -> str_equal, str_len, if, buffer_resize

Level 2:
  buffer_init -> max, malloc
  buffer_resize -> abs_val, min, realloc
  format_escape -> if, if
  parse_string -> if, str_copy
  validate_length -> min, str_len

Level 1 (Leaves):
  [abs_val,max,min,str_copy,str_equal,str_len]

Note: Library functions (malloc, realloc, if) are included in the call lists
      but are not user-defined functions, so they don't appear in the level structure.
```

## Test Data Details

### Dependency Graph Visualization

```
Level 6 (Top):
  main → initialize, process, cleanup

Level 5:
  initialize → processor_init, analyzer_setup
  process → processor_run, analyzer_check
  cleanup → processor_finalize, analyzer_report
  error_handler → format_escape, validate_content

Level 4:
  processor_init → parse_init, format_init
  processor_run → parse_token, format_output
  processor_finalize → parse_string, format_escape
  analyzer_setup → parse_init, format_init
  analyzer_check → parse_token, parse_string
  analyzer_report → format_output, format_escape

Level 3:
  parse_init → buffer_init
  parse_token → validate_length, buffer_resize
  parse_string → validate_content, str_copy
  format_init → buffer_init
  format_output → validate_length, buffer_resize
  format_escape → validate_content, str_equal

Level 2:
  buffer_init → max
  buffer_resize → min, abs_val
  validate_length → str_len, min
  validate_content → str_equal, str_len

Level 1 (Leaves):
  min, max, abs_val, str_len, str_equal, str_copy
  (No dependencies - pure leaf functions)
```

## Troubleshooting

### Test Failures

If tests fail, check:

1. **Cscope data generation**: Look at `logs/cscope_generation.log`
2. **Prolog import**: Check `logs/cscope_import_errors.log`
3. **Function counts**: Verify all 27 functions are imported
4. **Path issues**: Ensure Windows paths are correct (MSYS2, SWI-Prolog)

### Common Issues

- **"cscope not found"**: Add MSYS2 to PATH: `export PATH="/c/msys64/usr/bin:$PATH"`
- **"swipl not found"**: Add SWI-Prolog to PATH: `export PATH="/c/Program Files/swipl/bin:$PATH"`
- **Parse errors**: Check that files are UTF-8 encoded and tab-delimited
- **Wrong counts**: Verify cscope database was built correctly in `test/data/basic-deps/src/`

## Adding New Tests

To add new test cases:

1. Create new directory: `test/data/<test-name>/`
2. Add C source files with clear dependency structure
3. Create test script: `test/scripts/run_<test-name>_test.sh`
4. Document expected results in this README
5. Update `.gitignore` to exclude generated data if needed
