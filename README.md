# Prolog C Source Dependency Generator

A toolkit for analyzing C source code dependencies.

## Overview

This project provides tools to:
1. **Extract dependencies** from C code using cscope (Python or Prolog DCG-based)
2. **Import** data into a persistent Prolog database
3. **Analyze** symbol-level dependency graphs with powerful queries
4. **Visualize** relationships with interactive Cytoscape.js graphs

## Quick Start

```bash
# 1. Generate dependency data from C source
python src/python/generate_cscope_data.py --src /path/to/source --root .

# 2. Start Prolog and import (first time only)
swipl
?- ['src/prolog/cscope_import'].
Created new database: knowledge/cscope_facts.db

?- import_cscope_symbols('data/extracted/cscope_symbols.txt', []).
import_cscope_symbols: processed=25530, imported=25530, failed=0

?- import_cscope_defs('data/extracted/cscope_definitions.txt', []).
import_cscope_defs: processed=1523, imported=1523, failed=0

?- import_cscope_calls('data/extracted/cscope_callees.txt', []).
import_cscope_calls: processed=4892, imported=4892, failed=0

# 3. Query and analyze (works in all sessions - data persists!)
?- symbols_matching("parse", Symbol).           % Find parser-related symbols
?- leaf_symbols(Symbol).                        % Find functions with no dependencies
?- export_to_cytoscape('graph.json', [filter("parse")]).  % Export filtered graph
```

**Subsequent sessions:** Data automatically loads from the persistent database - no need to re-import!
```prolog
swipl
?- ['src/prolog/cscope_import'].
Loading database (0.3 MB)...
Database statistics:
  Symbols:     25530
  Definitions: 1523
  Calls:       4892
true.

% Query immediately without re-importing!
```

**For complete instructions including data extraction options, database management, Windows setup, troubleshooting, and advanced usage, see the [Complete Workflow Guide](docs/cscope-workflow.md).**

## Project Structure

```
prolog-c-source-dependency-generator/
├── src/
│   ├── prolog/
│   │   ├── cscope_extract.pl      # Prolog DCG-based extraction (new!)
│   │   ├── cscope_db.pl           # Persistent database layer (new!)
│   │   ├── cscope_import.pl       # Main importer with analysis predicates
│   │   └── deps_import_dcg.pl     # Legacy make/ctags importer
│   └── python/
│       └── generate_cscope_data.py # Python cscope data extraction
├── test/
│   ├── prolog/                    # Prolog unit tests (new!)
│   │   ├── test_cscope_extract_dcg.pl  # DCG extraction tests
│   │   └── test_cscope_import_dcg.pl   # Import DCG tests
│   ├── scripts/
│   │   └── run_basic_deps_test.py      # Integration test runner
│   └── README.md                  # Testing documentation
├── fixtures/                      # Test data (new!)
│   └── basic-deps/                # 6-level DAG test case
├── viewer/
│   └── index.html                 # Interactive Cytoscape.js visualization
├── docs/
│   └── cscope-workflow.md         # Complete workflow guide
├── data/
│   ├── extracted/                 # Extracted text data (gitignored)
│   └── legacy/                    # Old format data (gitignored)
├── logs/                          # Log files (gitignored)
├── knowledge/                     # Prolog databases & exports (gitignored)
│   └── cscope_facts.db            # Persistent database file
├── LICENSE
└── README.md                      # This file
```

## Requirements

- **Python 3.7+**: For running the generation script
- **Git Bash** (Windows): For running bash commands
  - Comes with Git for Windows: https://git-scm.com/download/win
- **cscope**: For extracting C source dependencies
  - Linux/Mac: Usually pre-installed
  - Windows: Install via MSYS2: `pacman -S msys/cscope`
- **SWI-Prolog**: For running analysis
  - https://www.swi-prolog.org/
- **Modern web browser**: For viewing Cytoscape.js graphs

## Documentation

- [Complete Workflow Guide](docs/cscope-workflow.md) - Step-by-step instructions, Windows setup, troubleshooting, and query examples
- [Interactive Viewer](viewer/index.html) - Cytoscape.js visualization (open in browser)

## License

See LICENSE file.