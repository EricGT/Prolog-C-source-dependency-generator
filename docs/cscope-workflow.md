# Cscope-based C Dependency Analysis Workflow

## Overview

This workflow uses cscope to extract symbol-level dependencies from C source code, import them into Prolog, analyze the dependency graph, and export to Cytoscape.js for interactive visualization.

## Prerequisites

- **Python 3.7+** for running the generation script
- **Git Bash** (Windows users) for bash commands - comes with Git for Windows
- **cscope** (standard on Linux/macOS, install via MSYS2 on Windows: `pacman -S msys/cscope`)
- **SWI-Prolog** with http/json library
- SQLite source code at: https://sqlite.org/2025/sqlite-src-3510000.zip - uses the `src` directory

**Windows users:**
1. Open Git Bash for all bash commands in this guide
2. After installing cscope via MSYS2, add MSYS2 to your PATH:
   ```bash
   export PATH="/c/msys64/usr/bin:$PATH"
   ```
   This makes cscope available in Git Bash for the current session.

## Step 1: Generate Cscope Data

Use the automated Python script to generate all cscope data:

```bash
# Ensure you're in the project root directory (Git Bash)
cd ~/Projects/Prolog-C-source-dependency-generator

# Generate data with debug output
# Note: Use forward slashes in paths (Git Bash style)
python src/python/generate_cscope_data.py \
  --src ~/Projects/SqliteVdbe/reference/sqlite-src-3510000 \
  --root . \
  --debug 1

# Or use interactive mode (will prompt for paths)
python src/python/generate_cscope_data.py
```

This will:
1. Build the cscope database from the SQLite `src/` directory
   - Creates `cscope.files`, `cscope.out`, `cscope.in.out`, `cscope.po.out` in the source `src/` directory
   - These database files remain in the source directory because cscope expects them there for queries
2. Extract definitions, function calls, symbols, and includes
3. Place extracted text files in `data/extracted/`
4. Write logs to `logs/`

### Cscope Output Format

Each line is tab-separated:
```
<filename>	<symbol>	<line_number> <context_line>
```

**Example from `cscope_definitions.txt`:**
```
src/vdbe.c	sqlite3VdbeExec	1234 int sqlite3VdbeExec(Vdbe *p){
src/vdbeInt.h	Vdbe	89 struct Vdbe {
src/vdbe.c	OPFLG_JUMP	42 #define OPFLG_JUMP 0x01
```

**Example from `cscope_callees.txt`:**
```
src/vdbe.c	sqlite3VdbeExec	1456 sqlite3VdbeMemSetInt64(pOut, pOp->p1);
src/vdbe.c	sqlite3VdbeExec	1789 rc = sqlite3VdbeMemGrow(pMem, n, 0);
```

## Step 2: Import into Prolog

### Starting SWI-Prolog

**From Git Bash (Windows):**

If `swipl` is in your PATH:
```bash
swipl
```

If `swipl` is not in your PATH, add it temporarily:
```bash
export PATH="/c/Program Files/swipl/bin:$PATH"
swipl
```

Or call it directly with the full path:
```bash
"/c/Program Files/swipl/bin/swipl.exe"
```

**Linux/macOS:**
```bash
swipl
```

### Load and Import Data

Once in SWI-Prolog, load the cscope_import module and import the data:

```prolog
?- ['src/prolog/cscope_import'].

% Import definitions (creates def/5 facts)
?- import_cscope_defs(
     'data/extracted/cscope_definitions.txt',
     user,
     [debug(true)]
   ).
% Output: import_cscope_defs: processed 1523 lines, succeeded 1523, failed 0

% Import call relationships (creates calls/5 facts)
?- import_cscope_calls(
     'data/extracted/cscope_callees.txt',
     user,
     [debug(true)]
   ).
% Output: import_cscope_calls: processed 4892 lines, succeeded 4892, failed 0
```

**Note:** Error logs (if any) will be written to `logs/cscope_import_errors.log`

## Step 3: Analyze Dependencies

### Query Examples

**Find all leaf functions (no dependencies):**
```prolog
?- leaf_symbols_in(user, Symbol).
Symbol = sqlite3VdbeMemGrow ;
Symbol = sqlite3VdbeMemSetInt64 ;
Symbol = sqlite3VdbeMemSetNull ;
...
```

**Find what a function calls:**
```prolog
?- calls_in(user, 'sqlite3VdbeExec', Callee).
Callee = sqlite3VdbeMemSetInt64 ;
Callee = sqlite3VdbeMemGrow ;
Callee = sqlite3VdbeSerialGet ;
...
```

**Get full dependency cone (transitive):**
```prolog
?- dependency_cone_in(user, 'sqlite3VdbeExec', Deps).
Deps = [sqlite3VdbeMemGrow, sqlite3VdbeMemSetInt64, ...].
```

**Find symbols matching a pattern:**
```prolog
?- symbols_matching(user, "vdbe", Symbol).  % SQLite VDBE symbols
Symbol = sqlite3VdbeExec ;
Symbol = sqlite3VdbeMemGrow ;
Symbol = Vdbe ;
...

?- symbols_matching(user, "parse", Symbol).  % Parser symbols
?- symbols_matching(user, "Btree", Symbol).  % B-tree symbols
```

**Get topologically sorted order (leaf-first):**
```prolog
?- conversion_order_in(user, Order).
Order = [sqlite3VdbeMemGrow, sqlite3VdbeMemSetInt64, ..., sqlite3VdbeExec].
```

**Find symbols matching a pattern in topological order:**
```prolog
?- findall(S, symbols_matching(user, "vdbe", S), MatchedSyms),
   conversion_order_in(user, AllOrder),
   intersection(AllOrder, MatchedSyms, FilteredOrder).
FilteredOrder = [...].  % Matched symbols in leaf-first order
```

**Inspect a specific symbol:**
```prolog
?- symbol_def_in(user, File, 'sqlite3VdbeExec', Line, Kind).
File = 'c:/users/groot/projects/sqlitevdbe/reference/sqlite-src-3510000/src/vdbe.c',
Line = 1234,
Kind = function.
```

## Step 4: Export to Cytoscape.js

Export the dependency graph for interactive visualization:

```prolog
% Export symbols matching a pattern (e.g., VDBE-related)
?- export_to_cytoscape(user,
     'knowledge/vdbe_deps.json',
     [filter("vdbe")]
   ).

% Export parser-related symbols
?- export_to_cytoscape(user,
     'knowledge/parser_deps.json',
     [filter("parse")]
   ).

% Export all functions (may be large!)
?- export_to_cytoscape(user,
     'knowledge/all_deps.json',
     [filter(all), max_nodes(100)]
   ).
```

### Cytoscape.js JSON Format

The output is a standard Cytoscape.js elements JSON:

```json
{
  "elements": {
    "nodes": [
      {
        "data": {
          "id": "sqlite3VdbeExec",
          "label": "sqlite3VdbeExec",
          "file": "c:/users/groot/.../src/vdbe.c",
          "line": 1234,
          "kind": "function"
        }
      },
      ...
    ],
    "edges": [
      {
        "data": {
          "id": "sqlite3VdbeExec_sqlite3VdbeMemGrow",
          "source": "sqlite3VdbeExec",
          "target": "sqlite3VdbeMemGrow"
        }
      },
      ...
    ]
  }
}
```

## Step 5: Visualize with Cytoscape.js

See `viewer/index.html` for a complete interactive visualization application.

### Quick Start

1. Open `viewer/index.html` in a browser
2. Load your exported JSON file (e.g., `knowledge/vdbe_deps.json`)
3. Interact with the graph:
   - **Zoom**: Mouse wheel
   - **Pan**: Click and drag
   - **Select node**: Click
   - **Info panel**: Shows file, line, dependencies
   - **Search**: Filter by symbol name
   - **Layout**: Switch between hierarchical, circle, grid

## Prolog Fact Schema Reference

### def/5 - Symbol Definitions
```prolog
def(File, Symbol, Line, Context, Kind).
```
- **File**: Normalized lowercase file path (forward slashes)
- **Symbol**: Atom (function name, struct name, macro name)
- **Line**: Integer line number
- **Context**: String (source line text)
- **Kind**: function | struct | typedef | enum | macro | unknown

### calls/5 - Function Call Relationships
```prolog
calls(File, Caller, Line, Context, Callee).
```
- **File**: Where the call occurs
- **Caller**: Function making the call
- **Line**: Line number of call
- **Context**: String (source line containing call)
- **Callee**: Function being called

### symbol/4 - Symbol References (optional)
```prolog
symbol(File, Symbol, Line, Context).
```
General symbol usage (from `cscope -L0`).

## Tips for Dependency Analysis

### Identify Entry Points
```prolog
% Find entry points for a specific subsystem (e.g., "vdbe", "parse", "btree")
?- symbols_matching(user, "vdbe", S),
   \+ calls_in(user, _, S),
   calls_in(user, S, _).
```
Finds symbols matching the pattern that are called by external code but call other functions (likely entry points).

### Find Isolated Subgraphs
```prolog
% Find leaf symbols with few callers (good starting points)
?- symbols_matching(user, "vdbe", S),
   leaf_symbols_in(user, S),
   findall(Caller, calls_in(user, Caller, S), Callers),
   length(Callers, N),
   N < 3.
```
Finds leaf symbols matching the pattern with few callers (good starting points for analysis or refactoring).

### Measure Complexity
```prolog
?- dependency_cone_in(user, 'sqlite3VdbeExec', Deps),
   length(Deps, N).
N = 247.  % This function transitively depends on 247 other symbols
```

### Export Subgraphs
To focus on a specific area, you can filter programmatically before exporting.

## Troubleshooting

### Empty Output Files
- Check your source directory's `src/` subdirectory for database files (`cscope.out`, `cscope.in.out`, `cscope.po.out`)
- Verify source files are in the `src/` subdirectory of your source tree
- Run with `--debug 2` for detailed diagnostics: `python src/python/generate_cscope_data.py --src /path --root . --debug 2`
- Check logs in `logs/cscope_generation.log`

### Parser Errors
- Check `logs/cscope_import_errors.log` for detailed parse failures
- Line format should be: `<file><TAB><symbol><TAB><num><SPACE><context>`
- Ensure UTF-8 encoding
- Use `stop_on_error(true)` option to stop at first error for debugging:
  ```prolog
  ?- import_cscope_defs('data/extracted/cscope_definitions.txt', user,
                        [debug(true), stop_on_error(true)]).
  ```

### Missing Dependencies
- Macro calls aren't tracked by cscope function call queries
- Struct field access doesn't create direct dependencies
- Check `data/extracted/cscope_symbols.txt` for broader symbol tracking

## Next Steps

1. **Analyze your subsystem**: Use `symbols_matching/3` with patterns relevant to your codebase (e.g., "vdbe", "parse", "btree")
2. **Add annotations**: Extend `def/5` facts with processing status (pending/done)
3. **Track progress**: Use dynamic predicates to mark analyzed symbols
4. **Generate reports**: Export analysis progress as CSV/HTML
5. **Customize filters**: Create domain-specific predicates that combine pattern matching with other criteria

Happy analyzing! 🚀
