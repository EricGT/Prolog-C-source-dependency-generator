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

### Understanding Persistence

**Important:** The system now uses persistent storage via SWI-Prolog's `library(persistency)`. Data is stored in `knowledge/cscope_facts.db` and **persists between Prolog sessions**.

**When to import:**
- **First time**: Database doesn't exist yet
- **After clearing**: Used `cscope_db:clear_db` to remove data
- **Source changed**: Regenerated cscope data files

**When NOT to import:**
- **Subsequent sessions**: The database auto-loads existing data when you load the module

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

### First Time: Load Module and Import Data

```prolog
?- ['src/prolog/cscope_import'].
% Creates new database: knowledge/cscope_facts.db
true.

% Import symbol references (creates symbol/4 facts in persistent database)
?- import_cscope_symbols(
     'data/extracted/cscope_symbols.txt',
     [debug(true)]
   ).
% Output: import_cscope_symbols: processed=25530, imported=25530, failed=0

% Import definitions (creates def/5 facts)
?- import_cscope_defs(
     'data/extracted/cscope_definitions.txt',
     [debug(true)]
   ).
% Output: import_cscope_defs: processed=1523, imported=1523, failed=0

% Import call relationships (creates calls/5 facts)
?- import_cscope_calls(
     'data/extracted/cscope_callees.txt',
     [debug(true)]
   ).
% Output: import_cscope_calls: processed=4892, imported=4892, failed=0

% Verify import
?- cscope_db:db_statistics.
Database statistics:
  Symbols:     25530
  Definitions: 1523
  Calls:       4892
```

### Subsequent Sessions: Auto-Load Existing Data

```prolog
?- ['src/prolog/cscope_import'].
Loading database (0.3 MB)...
Database statistics:
  Symbols:     25530
  Definitions: 1523
  Calls:       4892
true.

% Data is already loaded! You can query immediately without re-importing.
```

### Database Management

```prolog
% View database statistics
?- cscope_db:db_statistics.
Database statistics:
  Symbols:     25530
  Definitions: 1523
  Calls:       4892

% Clear database (removes all data - you'll need to re-import)
?- cscope_db:clear_db.
Database cleared.

% Compact journal (optimizes file size, keeps data)
?- cscope_db:compact_journal.
Journal compacted.
```

**Note:** Error logs (if any) will be written to `logs/cscope_import_errors.log`

## Step 3: Analyze Dependencies

### Query Examples

**Find all leaf functions (no dependencies):**
```prolog
?- leaf_symbols(Symbol).
Symbol = sqlite3VdbeMemGrow ;
Symbol = sqlite3VdbeMemSetInt64 ;
Symbol = sqlite3VdbeMemSetNull ;
...
```

**Find what a function calls:**
```prolog
?- calls('sqlite3VdbeExec', Callee).
Callee = sqlite3VdbeMemSetInt64 ;
Callee = sqlite3VdbeMemGrow ;
Callee = sqlite3VdbeSerialGet ;
...
```

**Get full dependency cone (transitive):**
```prolog
?- dependency_cone('sqlite3VdbeExec', Deps).
Deps = [sqlite3VdbeMemGrow, sqlite3VdbeMemSetInt64, ...].
```

**Find symbols matching a pattern:**
```prolog
?- symbols_matching("vdbe", Symbol).  % SQLite VDBE symbols
Symbol = sqlite3VdbeExec ;
Symbol = sqlite3VdbeMemGrow ;
Symbol = Vdbe ;
...

?- symbols_matching("parse", Symbol).  % Parser symbols
?- symbols_matching("Btree", Symbol).  % B-tree symbols
```

**Get topologically sorted order (leaf-first):**
```prolog
?- conversion_order(Order).
Order = [sqlite3VdbeMemGrow, sqlite3VdbeMemSetInt64, ..., sqlite3VdbeExec].
```

**Find symbols matching a pattern in topological order:**
```prolog
?- findall(S, symbols_matching("vdbe", S), MatchedSyms),
   conversion_order(AllOrder),
   intersection(AllOrder, MatchedSyms, FilteredOrder).
FilteredOrder = [...].  % Matched symbols in leaf-first order
```

**Inspect a specific symbol:**
```prolog
?- symbol_def(File, 'sqlite3VdbeExec', Line, Kind).
File = 'c:/users/groot/projects/sqlitevdbe/reference/sqlite-src-3510000/src/vdbe.c',
Line = 1234,
Kind = function.
```

## Step 4: Export to Cytoscape.js

Export the dependency graph for interactive visualization:

```prolog
% Export symbols matching a pattern (e.g., VDBE-related)
?- export_to_cytoscape(
     'knowledge/vdbe_deps.json',
     [filter("vdbe")]
   ).

% Export parser-related symbols
?- export_to_cytoscape(
     'knowledge/parser_deps.json',
     [filter("parse")]
   ).

% Export all functions (may be large!)
?- export_to_cytoscape(
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

### Viewing the Graph Locally

Due to CORS (Cross-Origin Resource Sharing) restrictions, browsers block loading JSON files via the `file://` protocol. You must serve the files through a local HTTP server.

**1. Start a local HTTP server:**

```bash
# From project root directory
python -m http.server 8000
```

You should see output like:
```
Serving HTTP on :: port 8000 (http://[::]:8000/) ...
```

**2. Open the viewer in your browser:**

Navigate to: **`http://localhost:8000/viewer/index.html`**

**Alternative access:**
- From another device on the same network: `http://<your-ip>:8000/viewer/index.html`
  (Replace `<your-ip>` with your computer's local IP address, e.g., `192.168.1.100`)

**3. Using the viewer:**

The viewer will automatically load `knowledge/all_deps.json` by default.

**Important notes:**
- The terminal running the HTTP server must stay open while using the viewer
- To stop the server: Press `Ctrl+C` in the terminal
- If you see errors, press `F12` to open browser DevTools and check the Console tab for error messages

**Expected server log output:**
```
::1 - - [08/Nov/2025 01:41:48] "GET /viewer/index.html HTTP/1.1" 200 -
::1 - - [08/Nov/2025 01:41:48] "GET /knowledge/all_deps.json HTTP/1.1" 200 -
::1 - - [08/Nov/2025 01:47:29] "GET /favicon.ico HTTP/1.1" 200 -
```

Status code `200` indicates successful file loading. If you see `404`, the file was not found.

### Quick Start

1. Ensure `knowledge/all_deps.json` exists (export from Prolog first if needed)
2. Start the HTTP server (see above)
3. Open `http://localhost:8000/viewer/index.html`
4. Interact with the graph:
   - **Zoom**: Mouse wheel
   - **Pan**: Click and drag
   - **Select node**: Click
   - **Info panel**: Shows file, line, dependencies
   - **Search**: Filter by symbol name
   - **Layout**: Switch between hierarchical, circle, grid

## Prolog Fact Schema Reference

### Persistent Database Storage

All facts are stored in `knowledge/cscope_facts.db` using SWI-Prolog's `library(persistency)`.

**Access predicates:**
- Query: `cscope_db:def(...)`, `cscope_db:calls(...)`, `cscope_db:symbol(...)`
- Assert: `cscope_db:assert_def(...)`, etc.
- Retract: `cscope_db:retract_def(...)`, etc.

**High-level API** (recommended):
- Use predicates from `cscope_import` module: `calls/2`, `leaf_symbols/1`, `dependency_cone/2`, etc.
- These automatically query the persistent database

### def/5 - Symbol Definitions
```prolog
cscope_db:def(File, Symbol, Line, Context, Kind).
```
- **File**: Normalized lowercase file path (forward slashes)
- **Symbol**: Atom (function name, struct name, macro name)
- **Line**: Integer line number
- **Context**: String (source line text)
- **Kind**: Atom - function | struct | typedef | enum | macro | unknown

**High-level access:**
```prolog
?- symbol_def(File, Symbol, Line, Kind).  % Uses persistent database
```

### calls/5 - Function Call Relationships
```prolog
cscope_db:calls(File, Caller, Line, Context, Callee).
```
- **File**: Where the call occurs
- **Caller**: Function making the call
- **Line**: Line number of call
- **Context**: String (source line containing call)
- **Callee**: Function being called

**High-level access:**
```prolog
?- calls(Caller, Callee).           % Simple 2-arg form
?- calls_transitive(Caller, Callee). % Transitive closure
```

### symbol/4 - Symbol References
```prolog
cscope_db:symbol(File, Symbol, Line, Context).
```
General symbol usage (from `cscope -L0`). All symbol occurrences in the codebase.

## Tips for Dependency Analysis

### Identify Entry Points
```prolog
% Find entry points for a specific subsystem (e.g., "vdbe", "parse", "btree")
?- symbols_matching("vdbe", S),
   \+ calls(_, S),
   calls(S, _).
```
Finds symbols matching the pattern that are called by external code but call other functions (likely entry points).

### Find Isolated Subgraphs
```prolog
% Find leaf symbols with few callers (good starting points)
?- symbols_matching("vdbe", S),
   leaf_symbols(S),
   findall(Caller, calls(Caller, S), Callers),
   length(Callers, N),
   N < 3.
```
Finds leaf symbols matching the pattern with few callers (good starting points for analysis or refactoring).

### Measure Complexity
```prolog
?- dependency_cone('sqlite3VdbeExec', Deps),
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
- Line format should be: `<file><TAB><symbol><TAB><num><TAB><context>` (all fields tab-separated)
- Ensure UTF-8 encoding
- Use `stop_on_error(true)` option to stop at first error for debugging:
  ```prolog
  ?- import_cscope_symbols('data/extracted/cscope_symbols.txt',
                          [debug(true), stop_on_error(true)]).
  ?- import_cscope_defs('data/extracted/cscope_definitions.txt',
                        [debug(true), stop_on_error(true)]).
  ?- import_cscope_calls('data/extracted/cscope_callees.txt',
                        [debug(true), stop_on_error(true)]).
  ```

### Database Issues
- If database gets corrupted: Delete `knowledge/cscope_facts.db` and re-import
- If database is large: Use `cscope_db:compact_journal` to optimize
- To start fresh: `cscope_db:clear_db` then re-import data

### Missing Dependencies
- Macro calls aren't tracked by cscope function call queries
- Struct field access doesn't create direct dependencies
- Check `data/extracted/cscope_symbols.txt` for broader symbol tracking

## Workflow Summary

### First Time or After Source Changes
```
1. python src/python/generate_cscope_data.py --src <path> --root .
2. swipl
3. ['src/prolog/cscope_import'].
4. import_cscope_symbols('data/extracted/cscope_symbols.txt', []).
5. import_cscope_defs('data/extracted/cscope_definitions.txt', []).
6. import_cscope_calls('data/extracted/cscope_callees.txt', []).
7. Run queries (calls/2, leaf_symbols/1, dependency_cone/2, etc.)
```

### Subsequent Sessions (Data Already Loaded)
```
1. swipl
2. ['src/prolog/cscope_import'].  % Auto-loads from knowledge/cscope_facts.db
3. Run queries immediately!
```

### When to Re-Import
- Database doesn't exist yet (first time)
- Used `cscope_db:clear_db` to remove data
- Source code changed and you regenerated cscope data

## Next Steps

1. **Analyze your subsystem**: Use `symbols_matching/2` with patterns relevant to your codebase (e.g., "vdbe", "parse", "btree")
2. **Export visualizations**: Use `export_to_cytoscape/2` to create interactive graphs
3. **Track progress**: The persistent database makes it easy to work incrementally
4. **Generate reports**: Export analysis progress as CSV/HTML
5. **Customize filters**: Create domain-specific predicates that combine pattern matching with other criteria

Happy analyzing! 🚀
