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
cscope_symbols.txt:108
cscope_callers.txt:42
cscope_includes.txt:26
[2/3] Importing data and running Prolog tests...
Created new database: knowledge/cscope_facts.db
import_cscope_symbols: processed=109, imported=108, failed=0
import_cscope_defs: processed=28, imported=27, failed=0
import_cscope_calls: processed=46, imported=45, failed=0
Database statistics:
  Symbols:     108
  Definitions: 27
  Calls:       45

[PASS] Found 6 leaf functions
[PASS] Topological order computed
       (Order length: 67 functions)
[PASS] main has 24 dependencies
[PASS] processor_init calls 2 functions (DAG structure)
[PASS] main transitively calls min (6 levels deep)

[3/3] All tests passed!
```

**Note:** The test now uses persistent storage via `library(persistency)`. Data is stored in `knowledge/cscope_facts.db` and persists between sessions. The import counts show:
- 108 symbols (all symbol references in the codebase)
- 27 definitions (all function definitions)
- 45 calls (all function call relationships)

## Manual Testing

You can also run queries manually:

```bash
# Generate data
python src/python/generate_cscope_data.py --src test/data/basic-deps --root .

# Start Prolog
swipl
```

### First Time Setup (or after database cleared)

```prolog
?- ['src/prolog/cscope_import'].
% The database auto-loads if it exists. If empty or doesn't exist, import data:

?- import_cscope_symbols('data/extracted/cscope_symbols.txt', []).
?- import_cscope_defs('data/extracted/cscope_definitions.txt', []).
?- import_cscope_calls('data/extracted/cscope_callees.txt', []).
```

### Subsequent Sessions (database already populated)

```prolog
?- ['src/prolog/cscope_import'].
Loading database (0.0 MB)...
Database statistics:
  Symbols:     108
  Definitions: 27
  Calls:       45
true.

% Data is already loaded! You can query immediately without re-importing.
```

### Querying the Database

```prolog
% Find all leaf functions (should be 6)
?- findall(S, leaf_symbols(S), Leaves), length(Leaves, N).
N = 6,
Leaves = [min, max, abs_val, str_len, str_equal, str_copy].

% Get topological order (includes all symbols, may have duplicates)
?- conversion_order(Order), length(Order, Len).
Len = 67.
% Note: Order includes library functions (malloc, realloc, if) and may have duplicates

% Analyze main's dependencies (should be 24)
?- dependency_cone(main, Deps), length(Deps, N).
N = 24.
% Includes: 21 user functions + 3 library functions (malloc, realloc, if)

% Verify DAG structure - diamond dependency (multiple paths to same function)
?- findall(C, calls(processor_init, C), Callees).
Callees = [parse_init, format_init].
% Both parse_init and format_init call buffer_init (converging paths)
?- findall(C, calls(parse_init, C), ParseCallees).
ParseCallees = [buffer_init].
?- findall(C, calls(format_init, C), FormatCallees).
FormatCallees = [buffer_init].
% Diamond structure:
%   processor_init
%       ├── parse_init ──┐
%       └── format_init ─┴──> buffer_init

% Test transitive calls (6 levels deep)
?- calls_transitive(main, min).
true.
% Note: May return multiple solutions if there are multiple call paths

% View database statistics
?- cscope_db:db_statistics.
Database statistics:
  Symbols:     108
  Definitions: 27
  Calls:       45

% Clear database (removes all data, you'll need to re-import)
?- cscope_db:clear_db.
Database cleared.

% Compact journal to reduce file size (keeps data, just optimizes storage)
?- cscope_db:compact_journal.
Journal compacted.

% Re-import after clearing or when data files change
?- import_cscope_symbols('data/extracted/cscope_symbols.txt', []).
?- import_cscope_defs('data/extracted/cscope_definitions.txt', []).
?- import_cscope_calls('data/extracted/cscope_callees.txt', []).
```

### Understanding Persistence

**Key Points:**
- The database is stored in `knowledge/cscope_facts.db` and **persists between Prolog sessions**
- When you load the module with `['src/prolog/cscope_import']`, it **automatically loads** existing data
- You only need to run `import_cscope_*` commands when:
  - The database doesn't exist yet (first time)
  - You cleared the database with `clear_db`
  - The source code changed and you regenerated cscope data
- The database is a journal file that grows over time - use `compact_journal/0` periodically to optimize it

**Typical Workflow:**

```
First time or after source changes:
  1. python src/python/generate_cscope_data.py --src <path> --root .
  2. swipl
  3. ['src/prolog/cscope_import'].  % Creates/loads database
  4. import_cscope_symbols(...).     % Import data (ONLY if new/changed)
  5. import_cscope_defs(...).
  6. import_cscope_calls(...).
  7. Run queries...

Subsequent sessions (data unchanged):
  1. swipl
  2. ['src/prolog/cscope_import'].  % Auto-loads existing data
  3. Run queries immediately!       % No import needed
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
