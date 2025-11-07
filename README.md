# Prolog C Source Dependency Generator

A toolkit for analyzing C source code dependencies.

## Overview

This project provides tools to:
1. **Extract dependencies** from C code using cscope and ctags
2. **Import** data into Prolog as queryable facts
3. **Analyze** symbol-level dependency graphs
4. **Visualize** relationships with interactive Cytoscape.js graphs

## Quick Start

```bash
# Generate dependency data from C source
python src/python/generate_cscope_data.py --src /path/to/source --root .

# Start Prolog and import
swipl
?- ['src/prolog/cscope_import'].
?- import_cscope_defs('data/extracted/cscope_definitions.txt', user, []).
?- import_cscope_calls('data/extracted/cscope_callees.txt', user, []).

# Query and analyze
?- symbols_matching(user, "parse", Symbol).     % Find parser-related symbols
?- leaf_symbols_in(user, Symbol).               % Find functions with no dependencies
?- export_to_cytoscape(user, 'graph.json', [filter("parse")]).  % Export filtered graph
```

**For complete instructions including Windows setup, troubleshooting, and advanced usage, see the [Complete Workflow Guide](docs/cscope-workflow.md).**

## Project Structure

```
prolog-c-source-dependency-generator/
├── src/
│   ├── prolog/
│   │   ├── cscope_import.pl       # Main cscope importer module
│   │   ├── deps_import_dcg.pl     # Legacy make/ctags importer
│   │   └── cscope_in_dcg.pl       # DCG utilities
│   └── python/
│       └── generate_cscope_data.py # Cscope data generation script
├── viewer/
│   └── index.html                 # Interactive Cytoscape.js visualization
├── docs/
│   └── cscope-workflow.md         # Complete workflow guide
├── data/
│   ├── extracted/                 # Extracted text data
│   └── legacy/                    # Old format data (tags, make deps)
├── logs/                          # Log files from generation/import
├── knowledge/                     # Prolog knowledge bases and exports
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