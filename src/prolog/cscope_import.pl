:- module(cscope_import, [
    % ---- Importers ----
    import_cscope_defs/3,           % +File, +IntoModule, +Options
    import_cscope_calls/3,          % +File, +IntoModule, +Options
    import_cscope_symbols/3,        % +File, +IntoModule, +Options

    % ---- Analysis predicates ----
    symbol_def_in/5,                % +Module, ?File, ?Symbol, ?Line, ?Kind
    calls_in/3,                     % +Module, ?Caller, ?Callee
    calls_transitive_in/3,          % +Module, ?Caller, ?Callee
    leaf_symbols_in/2,              % +Module, ?Symbol
    dependency_cone_in/3,           % +Module, +Symbol, -Deps
    symbols_matching/3,             % +Module, +Pattern, ?Symbol
    conversion_order_in/2,          % +Module, -OrderedSymbols

    % ---- Export to Cytoscape.js ----
    export_to_cytoscape/3           % +Module, +File, +Options
]).

/** <module> Cscope data import and dependency analysis

This module imports cscope line-oriented text output and builds a complete
symbol-level dependency graph suitable for analyzing C codebase dependencies.

Typical workflow:
  1. Generate cscope data (see module comments for commands)
  2. Import into Prolog facts
  3. Query dependencies
  4. Export to Cytoscape.js for visualization
  5. Use conversion_order_in/2 to get topologically sorted ordering (leaf-first)

Example:
  ?- import_cscope_defs('data/extracted/cscope_definitions.txt', user, [debug(true)]).
  ?- import_cscope_calls('data/extracted/cscope_callees.txt', user, [debug(true)]).
  ?- leaf_symbols_in(user, Sym).
  ?- symbols_matching(user, "parse", Sym).
  ?- export_to_cytoscape(user, 'knowledge/deps.json', [filter("parse")]).

Options:
  - debug(true/false)         : Enable debug output (default: false)
  - stop_on_error(true/false) : Stop on first parse error (default: false)
  - error_log(Path)           : Write parse errors to file (default: logs/cscope_import_errors.log)
*/

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(http/json)).
:- use_module(library(debug)).

% Enable debug topics
:- debug(cscope(parse)).
:- debug(cscope(import)).
:- debug(cscope(error)).
:- debug(cscope(sample)).

% ============================================================
% ==================  CSCOPE DATA IMPORT  ====================
% ============================================================

%% import_cscope_defs(+File, +IntoModule, +Options) is det.
%  Parse cscope -L1 output (global definitions) into def/5 facts using DCG.
%  Format: filename<TAB>symbol<TAB>line<SPACE>context
import_cscope_defs(Path, Mod, Opts) :-
    option(debug(Debug), Opts, false),
    option(stop_on_error(StopOnError), Opts, false),
    option(error_log(ErrorLog), Opts, 'logs/cscope_import_errors.log'),

    debug(cscope(import), 'Starting import_cscope_defs from ~w', [Path]),

    setup_call_cleanup(
        open(Path, read, S, [encoding(utf8)]),
        (   setup_call_cleanup(
                open(ErrorLog, write, ErrS, [encoding(utf8)]),
                import_defs_stream(S, ErrS, Mod, Debug, StopOnError, Stats),
                close(ErrS)
            )
        ),
        close(S)
    ),

    Stats = stats(Processed, Succeeded, Failed),
    debug(cscope(import), 'import_cscope_defs complete: processed=~d, succeeded=~d, failed=~d',
          [Processed, Succeeded, Failed]),
    (Debug == true ->
        format("import_cscope_defs: processed=~d, imported=~d, failed=~d~n",
               [Processed, Succeeded, Failed]),
        (Failed > 0 ->
            format("  Parse errors logged to: ~w~n", [ErrorLog])
        ; true)
    ; true).

%% import_defs_stream(+Stream, +ErrStream, +Module, +Debug, +StopOnError, -Stats) is det.
import_defs_stream(S, ErrS, Mod, Debug, StopOnError, stats(Processed, Succeeded, Failed)) :-
    import_defs_lines(S, ErrS, Mod, Debug, StopOnError, 1, 0, 0, Processed, Succeeded, Failed).

import_defs_lines(S, _ErrS, _Mod, _Debug, _StopOnError, LineNo, Succ0, Fail0, LineNo, Succ0, Fail0) :-
    at_end_of_stream(S),
    !.
import_defs_lines(S, ErrS, Mod, Debug, StopOnError, LineNo0, Succ0, Fail0, LineNoOut, SuccOut, FailOut) :-
    read_line_to_codes(S, Codes),
    (Codes == end_of_file ->
        LineNoOut = LineNo0, SuccOut = Succ0, FailOut = Fail0
    ;   (parse_def_line_dcg(Codes, File, Symbol, Line, Context, Kind, LineNo0) ->
            % Success
            assertz(Mod:def(File, Symbol, Line, Context, Kind)),
            Succ1 is Succ0 + 1,
            Fail1 = Fail0,
            (Debug == true, Succ1 =< 5 ->
                debug(cscope(sample), 'Sample def: ~w:~w at ~w (~w)', [File, Symbol, Line, Kind])
            ; true)
        ;   % Parse failed
            Fail1 is Fail0 + 1,
            Succ1 = Succ0,
            string_codes(LineStr, Codes),
            format(ErrS, "Line ~d: Parse failed~n  Content: ~s~n~n", [LineNo0, LineStr]),
            debug(cscope(error), 'Parse failed at line ~d: ~s', [LineNo0, LineStr]),
            (StopOnError == true ->
                throw(error(parse_error(def, LineNo0, LineStr), _))
            ; true)
        ),
        LineNo1 is LineNo0 + 1,
        import_defs_lines(S, ErrS, Mod, Debug, StopOnError, LineNo1, Succ1, Fail1, LineNoOut, SuccOut, FailOut)
    ).

%% import_cscope_calls(+File, +IntoModule, +Options) is det.
%  Parse cscope -L2 output (functions called by each function) using DCG.
%  Format: filename<TAB>caller<TAB>line<SPACE>context
import_cscope_calls(Path, Mod, Opts) :-
    option(debug(Debug), Opts, false),
    option(stop_on_error(StopOnError), Opts, false),
    option(error_log(ErrorLog), Opts, 'logs/cscope_import_errors.log'),

    debug(cscope(import), 'Starting import_cscope_calls from ~w', [Path]),

    setup_call_cleanup(
        open(Path, read, S, [encoding(utf8)]),
        (   setup_call_cleanup(
                open(ErrorLog, write, ErrS, [encoding(utf8)]),
                import_calls_stream(S, ErrS, Mod, Debug, StopOnError, Stats),
                close(ErrS)
            )
        ),
        close(S)
    ),

    Stats = stats(Processed, Succeeded, Failed),
    debug(cscope(import), 'import_cscope_calls complete: processed=~d, succeeded=~d, failed=~d',
          [Processed, Succeeded, Failed]),
    (Debug == true ->
        format("import_cscope_calls: processed=~d, imported=~d, failed=~d~n",
               [Processed, Succeeded, Failed]),
        (Failed > 0 ->
            format("  Parse errors logged to: ~w~n", [ErrorLog])
        ; true)
    ; true).

import_calls_stream(S, ErrS, Mod, Debug, StopOnError, stats(Processed, Succeeded, Failed)) :-
    import_calls_lines(S, ErrS, Mod, Debug, StopOnError, 1, 0, 0, Processed, Succeeded, Failed).

import_calls_lines(S, _ErrS, _Mod, _Debug, _StopOnError, LineNo, Succ0, Fail0, LineNo, Succ0, Fail0) :-
    at_end_of_stream(S),
    !.
import_calls_lines(S, ErrS, Mod, Debug, StopOnError, LineNo0, Succ0, Fail0, LineNoOut, SuccOut, FailOut) :-
    read_line_to_codes(S, Codes),
    (Codes == end_of_file ->
        LineNoOut = LineNo0, SuccOut = Succ0, FailOut = Fail0
    ;   (parse_call_line_dcg(Codes, File, Caller, Line, Context, Callee, LineNo0) ->
            % Success
            assertz(Mod:calls(File, Caller, Line, Context, Callee)),
            Succ1 is Succ0 + 1,
            Fail1 = Fail0,
            (Debug == true, Succ1 =< 5 ->
                debug(cscope(sample), 'Sample call: ~w calls ~w at ~w:~w', [Caller, Callee, File, Line])
            ; true)
        ;   % Parse failed
            Fail1 is Fail0 + 1,
            Succ1 = Succ0,
            string_codes(LineStr, Codes),
            format(ErrS, "Line ~d: Parse failed~n  Content: ~s~n~n", [LineNo0, LineStr]),
            debug(cscope(error), 'Parse failed at line ~d: ~s', [LineNo0, LineStr]),
            (StopOnError == true ->
                throw(error(parse_error(calls, LineNo0, LineStr), _))
            ; true)
        ),
        LineNo1 is LineNo0 + 1,
        import_calls_lines(S, ErrS, Mod, Debug, StopOnError, LineNo1, Succ1, Fail1, LineNoOut, SuccOut, FailOut)
    ).

%% import_cscope_symbols(+File, +IntoModule, +Options) is det.
%  Parse cscope -L0 output (all symbol references) using DCG.
%  Format: filename<TAB>context_func<TAB>line<SPACE>context
import_cscope_symbols(Path, Mod, Opts) :-
    option(debug(Debug), Opts, false),
    option(stop_on_error(StopOnError), Opts, false),
    option(error_log(ErrorLog), Opts, 'logs/cscope_import_errors.log'),

    debug(cscope(import), 'Starting import_cscope_symbols from ~w', [Path]),

    setup_call_cleanup(
        open(Path, read, S, [encoding(utf8)]),
        (   setup_call_cleanup(
                open(ErrorLog, write, ErrS, [encoding(utf8)]),
                import_symbols_stream(S, ErrS, Mod, Debug, StopOnError, Stats),
                close(ErrS)
            )
        ),
        close(S)
    ),

    Stats = stats(Processed, Succeeded, Failed),
    debug(cscope(import), 'import_cscope_symbols complete: processed=~d, succeeded=~d, failed=~d',
          [Processed, Succeeded, Failed]),
    (Debug == true ->
        format("import_cscope_symbols: processed=~d, imported=~d, failed=~d~n",
               [Processed, Succeeded, Failed]),
        (Failed > 0 ->
            format("  Parse errors logged to: ~w~n", [ErrorLog])
        ; true)
    ; true).

import_symbols_stream(S, ErrS, Mod, Debug, StopOnError, stats(Processed, Succeeded, Failed)) :-
    import_symbols_lines(S, ErrS, Mod, Debug, StopOnError, 1, 0, 0, Processed, Succeeded, Failed).

import_symbols_lines(S, _ErrS, _Mod, _Debug, _StopOnError, LineNo, Succ0, Fail0, LineNo, Succ0, Fail0) :-
    at_end_of_stream(S),
    !.
import_symbols_lines(S, ErrS, Mod, Debug, StopOnError, LineNo0, Succ0, Fail0, LineNoOut, SuccOut, FailOut) :-
    read_line_to_codes(S, Codes),
    (Codes == end_of_file ->
        LineNoOut = LineNo0, SuccOut = Succ0, FailOut = Fail0
    ;   (parse_symbol_line_dcg(Codes, File, Symbol, Line, Context, LineNo0) ->
            % Success
            assertz(Mod:symbol(File, Symbol, Line, Context)),
            Succ1 is Succ0 + 1,
            Fail1 = Fail0,
            (Debug == true, Succ1 =< 5 ->
                debug(cscope(sample), 'Sample symbol: ~w in ~w at ~w', [Symbol, File, Line])
            ; true)
        ;   % Parse failed
            Fail1 is Fail0 + 1,
            Succ1 = Succ0,
            string_codes(LineStr, Codes),
            format(ErrS, "Line ~d: Parse failed~n  Content: ~s~n~n", [LineNo0, LineStr]),
            debug(cscope(error), 'Parse failed at line ~d: ~s', [LineNo0, LineStr]),
            (StopOnError == true ->
                throw(error(parse_error(symbols, LineNo0, LineStr), _))
            ; true)
        ),
        LineNo1 is LineNo0 + 1,
        import_symbols_lines(S, ErrS, Mod, Debug, StopOnError, LineNo1, Succ1, Fail1, LineNoOut, SuccOut, FailOut)
    ).

% ============================================================
% =================  DCG LINE PARSERS  =======================
% ============================================================

%% parse_def_line_dcg(+Codes, -File, -Symbol, -LineNum, -Context, -Kind, +LineNo) is semidet.
%  Parse: filename<TAB>symbol<TAB>line<SPACE>context
parse_def_line_dcg(Codes, File, Symbol, LineNum, Context, Kind, LineNo) :-
    debug(cscope(parse), 'Parsing def line ~d', [LineNo]),
    phrase(def_line(FileStr, SymbolStr, LineNum, ContextStr), Codes),
    !,
    normalize_path_string(FileStr, File),
    atom_string(Symbol, SymbolStr),
    atom_string(Context, ContextStr),
    infer_kind(ContextStr, Kind),
    debug(cscope(parse), '  Success: ~w:~w', [File, Symbol]).
parse_def_line_dcg(Codes, _, _, _, _, _, LineNo) :-
    debug(cscope(parse), 'Failed to parse def line ~d', [LineNo]),
    (debugging(cscope(parse)) ->
        string_codes(LineStr, Codes),
        debug(cscope(parse), '  Line content: ~s', [LineStr])
    ; true),
    fail.

%% def_line(-File, -Symbol, -LineNum, -Context)// is semidet.
def_line(File, Symbol, LineNum, Context) -->
    field_until_tab(FileCodes),
    { string_codes(File, FileCodes),
      debug(cscope(parse), '    Field 1 (file): ~s', [File]) },
    tab_char,
    field_until_tab(SymbolCodes),
    { string_codes(Symbol, SymbolCodes),
      debug(cscope(parse), '    Field 2 (symbol): ~s', [Symbol]) },
    tab_char,
    digits(LineNumCodes),
    { number_codes(LineNum, LineNumCodes),
      debug(cscope(parse), '    Field 3 (line): ~w', [LineNum]) },
    space_char,
    rest_of_line(ContextCodes),
    { string_codes(Context, ContextCodes),
      debug(cscope(parse), '    Field 4 (context): ~s', [Context]) }.

%% parse_call_line_dcg(+Codes, -File, -Caller, -LineNum, -Context, -Callee, +LineNo) is semidet.
%  Parse: filename<TAB>caller<TAB>line<SPACE>context
parse_call_line_dcg(Codes, File, Caller, LineNum, Context, Callee, LineNo) :-
    debug(cscope(parse), 'Parsing call line ~d', [LineNo]),
    phrase(call_line(FileStr, CallerStr, LineNum, ContextStr), Codes),
    !,
    normalize_path_string(FileStr, File),
    atom_string(Caller, CallerStr),
    atom_string(Context, ContextStr),
    extract_callee(ContextStr, Callee),
    debug(cscope(parse), '  Success: ~w calls ~w at ~w:~w', [Caller, Callee, File, LineNum]).
parse_call_line_dcg(Codes, _, _, _, _, _, LineNo) :-
    debug(cscope(parse), 'Failed to parse call line ~d', [LineNo]),
    (debugging(cscope(parse)) ->
        string_codes(LineStr, Codes),
        debug(cscope(parse), '  Line content: ~s', [LineStr])
    ; true),
    fail.

%% call_line(-File, -Caller, -LineNum, -Context)// is semidet.
call_line(File, Caller, LineNum, Context) -->
    field_until_tab(FileCodes),
    { string_codes(File, FileCodes),
      debug(cscope(parse), '    Field 1 (file): ~s', [File]) },
    tab_char,
    field_until_tab(CallerCodes),
    { string_codes(Caller, CallerCodes),
      debug(cscope(parse), '    Field 2 (caller): ~s', [Caller]) },
    tab_char,
    digits(LineNumCodes),
    { number_codes(LineNum, LineNumCodes),
      debug(cscope(parse), '    Field 3 (line): ~w', [LineNum]) },
    space_char,
    rest_of_line(ContextCodes),
    { string_codes(Context, ContextCodes),
      debug(cscope(parse), '    Field 4 (context): ~s', [Context]) }.

%% parse_symbol_line_dcg(+Codes, -File, -Symbol, -LineNum, -Context, +LineNo) is semidet.
%  Parse: filename<TAB>symbol<TAB>line<SPACE>context
parse_symbol_line_dcg(Codes, File, Symbol, LineNum, Context, LineNo) :-
    debug(cscope(parse), 'Parsing symbol line ~d', [LineNo]),
    phrase(symbol_line(FileStr, SymbolStr, LineNum, ContextStr), Codes),
    !,
    normalize_path_string(FileStr, File),
    atom_string(Symbol, SymbolStr),
    atom_string(Context, ContextStr),
    debug(cscope(parse), '  Success: ~w:~w at ~w', [File, Symbol, LineNum]).
parse_symbol_line_dcg(Codes, _, _, _, _, LineNo) :-
    debug(cscope(parse), 'Failed to parse symbol line ~d', [LineNo]),
    (debugging(cscope(parse)) ->
        string_codes(LineStr, Codes),
        debug(cscope(parse), '  Line content: ~s', [LineStr])
    ; true),
    fail.

%% symbol_line(-File, -Symbol, -LineNum, -Context)// is semidet.
symbol_line(File, Symbol, LineNum, Context) -->
    field_until_tab(FileCodes),
    { string_codes(File, FileCodes),
      debug(cscope(parse), '    Field 1 (file): ~s', [File]) },
    tab_char,
    field_until_tab(SymbolCodes),
    { string_codes(Symbol, SymbolCodes),
      debug(cscope(parse), '    Field 2 (symbol): ~s', [Symbol]) },
    tab_char,
    digits(LineNumCodes),
    { number_codes(LineNum, LineNumCodes),
      debug(cscope(parse), '    Field 3 (line): ~w', [LineNum]) },
    space_char,
    rest_of_line(ContextCodes),
    { string_codes(Context, ContextCodes),
      debug(cscope(parse), '    Field 4 (context): ~s', [Context]) }.

% ============================================================
% ================  DCG HELPER RULES  ========================
% ============================================================

%% tab_char// is det.
tab_char --> [9].  % ASCII tab

%% space_char// is det.
space_char --> [32].  % ASCII space

%% digit(?Code)// is semidet.
digit(C) --> [C], { code_type(C, digit) }.

%% digits(-Codes)// is semidet.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).

%% field_until_tab(-Codes)// is semidet.
%  Read all characters until TAB (not including TAB)
field_until_tab([C|Cs]) --> [C], { C \= 9 }, field_until_tab(Cs).
field_until_tab([]) --> [].

%% rest_of_line(-Codes)// is det.
%  Read all remaining characters (including any newlines at end)
rest_of_line([C|Cs]) --> [C], { C \= 10, C \= 13 }, rest_of_line(Cs).
rest_of_line([]) --> ([10]; [13]; [13, 10]; [10, 13]; []).

% ============================================================
% ===================  HELPER PREDICATES  ====================
% ============================================================

%% normalize_path_string(+PathStr, -NormalizedAtom) is det.
%  Convert file path to normalized lowercase atom with forward slashes
normalize_path_string(PathStr, NormAtom) :-
    string_codes(PathStr, Codes0),
    maplist(normalize_path_code, Codes0, Codes1),
    string_codes(NormStr, Codes1),
    string_lower(NormStr, Lower),
    atom_string(NormAtom, Lower).

normalize_path_code(92, 47) :- !.  % backslash -> forward slash
normalize_path_code(Code, Code).

%% infer_kind(+Context, -Kind) is det.
%  Heuristic: infer symbol kind from context line
infer_kind(Context, Kind) :-
    string_lower(Context, LC),
    (   sub_string(LC, _, _, _, "struct ") -> Kind = struct
    ;   sub_string(LC, _, _, _, "typedef ") -> Kind = typedef
    ;   sub_string(LC, _, _, _, "enum ") -> Kind = enum
    ;   sub_string(LC, _, _, _, "#define ") -> Kind = macro
    ;   sub_string(LC, _, _, _, "(") -> Kind = function
    ;   Kind = unknown
    ).

% TODO: Convert from using re_matchsub/6 to DCGs.

%% extract_callee(+Context, -Callee) is det.
%  Extract function name from call context using regex
extract_callee(Context, Callee) :-
    re_matchsub("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\(", Context, Sub, []),
    !,
    get_dict(1, Sub, CalleeStr),
    atom_string(Callee, CalleeStr).
extract_callee(_, unknown).  % fallback

% ============================================================
% ==================  ANALYSIS PREDICATES  ===================
% ============================================================

%% symbol_def_in(+Module, ?File, ?Symbol, ?Line, ?Kind) is nondet.
symbol_def_in(M, File, Symbol, Line, Kind) :-
    M:def(File, Symbol, Line, _, Kind).

%% calls_in(+Module, ?Caller, ?Callee) is nondet.
%  Direct function call relationship
calls_in(M, Caller, Callee) :-
    M:calls(_, Caller, _, _, Callee).

%% calls_transitive_in(+Module, ?Caller, ?Callee) is nondet.
%  Transitive closure of function calls
calls_transitive_in(M, Caller, Callee) :-
    calls_in(M, Caller, Callee).
calls_transitive_in(M, Caller, Callee) :-
    calls_in(M, Caller, Mid),
    calls_transitive_in(M, Mid, Callee).

%% leaf_symbols_in(+Module, ?Symbol) is nondet.
%  Symbols that don't call anything else (leaf nodes in dependency graph)
leaf_symbols_in(M, Symbol) :-
    M:def(_, Symbol, _, _, function),
    \+ M:calls(_, Symbol, _, _, _).

%% dependency_cone_in(+Module, +Symbol, -Deps) is det.
%  All symbols transitively called by Symbol, sorted
dependency_cone_in(M, Symbol, Deps) :-
    findall(Dep, calls_transitive_in(M, Symbol, Dep), DepList),
    sort(DepList, Deps).

%% symbols_matching(+Module, +Pattern, ?Symbol) is nondet.
%  Find symbols whose name contains Pattern (case-insensitive substring match)
%
%  @arg Module   The Prolog module containing the facts
%  @arg Pattern  String or atom to search for (case-insensitive)
%  @arg Symbol   Matched symbol name
%
%  Examples:
%    ?- symbols_matching(user, "vdbe", Symbol).     % SQLite VDBE symbols
%    ?- symbols_matching(user, "parse", Symbol).    % Parser-related symbols
%    ?- symbols_matching(user, "Btree", Symbol).    % B-tree symbols
symbols_matching(M, Pattern, Symbol) :-
    M:def(_, Symbol, _, _, _),
    atom_string(Symbol, SymStr),
    string_lower(SymStr, LowerSym),
    (   atom(Pattern) -> atom_string(Pattern, PatternStr) ; PatternStr = Pattern ),
    string_lower(PatternStr, LowerPattern),
    sub_string(LowerSym, _, _, _, LowerPattern).

%% conversion_order_in(+Module, -OrderedSymbols) is det.
%  Topologically sorted function list: leaves first, roots last
conversion_order_in(M, Ordered) :-
    findall(F, M:def(_, F, _, _, function), Funcs0),
    sort(Funcs0, Funcs),
    % Build adjacency list for topological sort
    maplist({M}/[F, F-Deps]>>findall(D, calls_in(M, F, D), Deps), Funcs, Pairs),
    topological_sort(Pairs, Ordered).

%% topological_sort(+AdjList, -Sorted) is det.
%  Simple topological sort using Kahn's algorithm
topological_sort(AdjList, Sorted) :-
    findall(V, member(V-_, AdjList), AllNodes0),
    findall(D, (member(_-Deps, AdjList), member(D, Deps)), AllDeps0),
    sort(AllNodes0, AllNodes),
    sort(AllDeps0, AllDeps),
    ord_subtract(AllNodes, AllDeps, Leaves),
    topo_visit(Leaves, AdjList, [], Sorted).

topo_visit([], _, Acc, Sorted) :- reverse(Acc, Sorted).
topo_visit([Node|Rest], AdjList, Acc, Sorted) :-
    member(Node-Deps, AdjList),
    !,
    subtract(Deps, [Node], FilteredDeps),  % remove self-cycles
    append(FilteredDeps, Rest, NewQueue0),
    sort(NewQueue0, NewQueue),
    topo_visit(NewQueue, AdjList, [Node|Acc], Sorted).
topo_visit([Node|Rest], AdjList, Acc, Sorted) :-
    topo_visit(Rest, AdjList, [Node|Acc], Sorted).

% ============================================================
% ===============  CYTOSCAPE.JS JSON EXPORT  =================
% ============================================================

%% export_to_cytoscape(+Module, +File, +Options) is det.
%  Export dependency graph to Cytoscape.js JSON format
%  Options:
%    - filter(Pattern)  : only include symbols matching Pattern (case-insensitive)
%    - filter(all)      : include all symbols (default)
%    - max_nodes(N)     : limit to N nodes (default: unlimited)
%
%  Examples:
%    ?- export_to_cytoscape(user, 'graph.json', [filter("vdbe")]).
%    ?- export_to_cytoscape(user, 'graph.json', [filter("parse"), max_nodes(50)]).
%    ?- export_to_cytoscape(user, 'graph.json', [filter(all)]).
export_to_cytoscape(M, File, Options) :-
    option(filter(Filter), Options, all),
    option(max_nodes(MaxNodes), Options, infinite),

    % Collect nodes
    (   Filter == all
    ->  findall(S, M:def(_, S, _, _, function), Symbols0)
    ;   findall(S, symbols_matching(M, Filter, S), Symbols0)
    ),
    sort(Symbols0, Symbols1),
    (   MaxNodes == infinite
    ->  Symbols = Symbols1
    ;   length(Prefix, MaxNodes),
        append(Prefix, _, Symbols1),
        Symbols = Prefix
    ),

    % Build node list
    maplist({M}/[Sym, Node]>>build_node(M, Sym, Node), Symbols, Nodes),

    % Build edge list
    findall(Edge,
            (member(Sym, Symbols),
             calls_in(M, Sym, Target),
             member(Target, Symbols),
             build_edge(Sym, Target, Edge)),
            Edges),

    % Create JSON structure
    JSON = _{elements: _{nodes: Nodes, edges: Edges}},

    % Write to file
    setup_call_cleanup(
        open(File, write, S, [encoding(utf8)]),
        json_write(S, JSON, [width(0)]),
        close(S)).

%% build_node(+Module, +Symbol, -NodeDict) is det.
build_node(M, Symbol, Node) :-
    M:def(File, Symbol, Line, _, Kind),
    !,
    atom_string(Symbol, SymbolStr),
    atom_string(File, FileStr),
    atom_string(Kind, KindStr),
    Node = _{data: _{id: SymbolStr,
                     label: SymbolStr,
                     file: FileStr,
                     line: Line,
                     kind: KindStr}}.
build_node(_, Symbol, Node) :-
    atom_string(Symbol, SymbolStr),
    Node = _{data: _{id: SymbolStr,
                     label: SymbolStr,
                     kind: "unknown"}}.

%% build_edge(+Source, +Target, -EdgeDict) is det.
build_edge(Source, Target, Edge) :-
    atom_string(Source, SourceStr),
    atom_string(Target, TargetStr),
    format(atom(EdgeId), "~w_~w", [Source, Target]),
    atom_string(EdgeId, EdgeIdStr),
    Edge = _{data: _{id: EdgeIdStr,
                     source: SourceStr,
                     target: TargetStr}}.

% ============================================================
% ====================  CONVENIENCE  =========================
% ============================================================

%% Export helpers for debugging
:- multifile user:portray/1.
user:portray(def(F,S,L,_,K)) :-
    format("def(~w, ~w, ~w, _, ~w)", [F,S,L,K]).
user:portray(calls(F,C,L,_,T)) :-
    format("calls(~w, ~w, ~w, _, ~w)", [F,C,L,T]).
