:- module(cscope_extract, [
    generate_cscope_data/2,    % generate_cscope_data(+SrcDir, +Options)
    extract_definitions/3,      % extract_definitions(+SymbolsFile, -Defs, +Options)
    extract_function_calls/3,   % extract_function_calls(+SymbolsFile, -Calls, +Options)
    test_has_paren/1            % test_has_paren(+String) - for debugging
]).

/** <module> Cscope Data Extraction with DCGs

This module provides Prolog-based extraction of cscope data, replacing
the Python generate_cscope_data.py script with pure Prolog + DCGs.

@author Prolog C Source Dependency Generator
@license MIT
*/

:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(filesex)).
:- use_module(library(dcg/basics)).

%! generate_cscope_data(+SrcDir, +Options) is det.
%
% Generate all cscope data files from source directory.
%
% Options:
%   - root(Path): Project root directory (default: '.')
%   - debug(Level): Debug level 0-3 (default: 0)
%   - quiet(Bool): Suppress output (default: false)
%
% @param SrcDir Source directory (must contain src/ subdirectory)
% @param Options Option list

generate_cscope_data(SrcDir, Options) :-
    % Parse options
    option(root(Root), Options, '.'),
    option(debug(DebugLevel), Options, 0),
    option(quiet(Quiet), Options, false),

    % Set up directories
    atom_concat(Root, '/data/extracted', ExtractedDir),
    atom_concat(Root, '/logs', LogDir),
    atom_concat(SrcDir, '/src', SrcSubdir),

    % Create directories
    make_directory_path(ExtractedDir),
    make_directory_path(LogDir),

    % Step 1: Build cscope database
    (Quiet = false -> format('[1/6] Building cscope database...~n', []) ; true),
    build_cscope_database(SrcSubdir, DebugLevel),

    % Step 2-5: Extract data
    (Quiet = false -> format('[2/6] Extracting symbols...~n', []) ; true),
    extract_symbols(SrcSubdir, ExtractedDir, SymbolCount, DebugLevel),

    (Quiet = false -> format('[3/6] Extracting callers...~n', []) ; true),
    extract_callers(SrcSubdir, ExtractedDir, CallerCount, DebugLevel),

    (Quiet = false -> format('[4/6] Extracting includes...~n', []) ; true),
    extract_includes(SrcSubdir, ExtractedDir, IncludeCount, DebugLevel),

    (Quiet = false -> format('[5/6] Extracting definitions...~n', []) ; true),
    atom_concat(ExtractedDir, '/cscope_symbols.txt', SymbolsFile),
    extract_definitions(SymbolsFile, Defs, [debug(DebugLevel)]),
    atom_concat(ExtractedDir, '/cscope_definitions.txt', DefsFile),
    write_definitions(DefsFile, Defs),
    length(Defs, DefCount),

    (Quiet = false -> format('[6/6] Extracting function calls...~n', []) ; true),
    extract_function_calls(SymbolsFile, Calls, [debug(DebugLevel), cscope_dir(SrcSubdir)]),
    atom_concat(ExtractedDir, '/cscope_callees.txt', CallsFile),
    write_calls(CallsFile, Calls),
    length(Calls, CallCount),

    % Summary
    (Quiet = true ->
        format('cscope_symbols.txt:~w~n', [SymbolCount]),
        format('cscope_callers.txt:~w~n', [CallerCount]),
        format('cscope_includes.txt:~w~n', [IncludeCount])
    ;
        format('~nSummary:~n', []),
        format('  Symbols:     ~w~n', [SymbolCount]),
        format('  Definitions: ~w~n', [DefCount]),
        format('  Calls:       ~w~n', [CallCount]),
        format('  Callers:     ~w~n', [CallerCount]),
        format('  Includes:    ~w~n', [IncludeCount])
    ).

%! build_cscope_database(+SrcDir, +DebugLevel) is det.
%
% Build cscope database in source directory.

build_cscope_database(SrcDir, DebugLevel) :-
    % Discover source files
    directory_files(SrcDir, AllFiles),
    include(is_source_file, AllFiles, SourceFiles),

    % Create cscope.files
    atom_concat(SrcDir, '/cscope.files', CscopeFiles),
    open(CscopeFiles, write, Out),
    forall(
        member(File, SourceFiles),
        (
            atom_concat(SrcDir, '/', SrcDir1),
            atom_concat(SrcDir1, File, FullPath),
            absolute_file_name(FullPath, AbsPath),
            format(Out, '~w~n', [AbsPath])
        )
    ),
    close(Out),

    % Build database
    (DebugLevel > 0 ->
        format('  Building database from ~w files~n', [SourceFiles])
    ; true),

    process_create(path(cscope), ['-b', '-k', '-q'],
                   [cwd(SrcDir), stdout(null), stderr(null)]).

%! is_source_file(+File) is semidet.
%
% True if File has a C source extension.

is_source_file(File) :-
    \+ atom_concat('.', _, File),  % Skip hidden files
    (
        atom_concat(_, '.c', File)
    ;   atom_concat(_, '.h', File)
    ;   atom_concat(_, '.cpp', File)
    ;   atom_concat(_, '.hpp', File)
    ).

%! extract_symbols(+SrcDir, +OutDir, -Count, +DebugLevel) is det.
%
% Extract symbol references using cscope -L -0.

extract_symbols(SrcDir, OutDir, Count, DebugLevel) :-
    atom_concat(OutDir, '/cscope_symbols.txt', OutFile),
    run_cscope_query(SrcDir, ['-d', '-L', '-0', '[a-zA-Z_].*'],
                     OutFile, Count, DebugLevel).

%! extract_callers(+SrcDir, +OutDir, -Count, +DebugLevel) is det.
%
% Extract reverse call relationships using cscope -L -3.

extract_callers(SrcDir, OutDir, Count, DebugLevel) :-
    atom_concat(OutDir, '/cscope_callers.txt', OutFile),
    run_cscope_query(SrcDir, ['-d', '-L', '-3', '[a-zA-Z_].*'],
                     OutFile, Count, DebugLevel).

%! extract_includes(+SrcDir, +OutDir, -Count, +DebugLevel) is det.
%
% Extract include relationships using cscope -L -8.

extract_includes(SrcDir, OutDir, Count, DebugLevel) :-
    atom_concat(OutDir, '/cscope_includes.txt', OutFile),
    run_cscope_query(SrcDir, ['-d', '-L', '-8', '[a-zA-Z_].*'],
                     OutFile, Count, DebugLevel).

%! run_cscope_query(+SrcDir, +Args, +OutFile, -LineCount, +DebugLevel) is det.
%
% Run cscope query and write reformatted output.

run_cscope_query(SrcDir, Args, OutFile, LineCount, DebugLevel) :-
    (DebugLevel > 1 ->
        format('  Running: cscope ~w~n', [Args])
    ; true),

    process_create(path(cscope), Args,
                   [cwd(SrcDir), stdout(pipe(In)), stderr(null)]),

    open(OutFile, write, Out),
    read_and_reformat_lines(In, Out, 0, LineCount),
    close(In),
    close(Out).

%! read_and_reformat_lines(+In, +Out, +Acc, -Total) is det.
%
% Read lines from cscope output, reformat to tab-separated, and write.

read_and_reformat_lines(In, Out, Acc, Total) :-
    read_line_to_string(In, Line),
    (   Line == end_of_file
    ->  Total = Acc
    ;   reformat_cscope_line(Line, Reformatted),
        format(Out, '~s~n', [Reformatted]),
        Acc1 is Acc + 1,
        read_and_reformat_lines(In, Out, Acc1, Total)
    ).

%! reformat_cscope_line(+Line, -Reformatted) is det.
%
% Reformat space-separated cscope output to tab-separated.

reformat_cscope_line(Line, Reformatted) :-
    split_string(Line, " ", "", [File, Symbol, LineNum | ContextParts]),
    atomics_to_string(ContextParts, " ", Context),
    normalize_path(File, NormFile),
    atomics_to_string([NormFile, Symbol, LineNum, Context], "\t", Reformatted).

%! normalize_path(+Path, -Normalized) is det.
%
% Normalize file path for Prolog (lowercase, forward slashes).

normalize_path(Path, Normalized) :-
    string_lower(Path, LowerStr),
    split_string(LowerStr, "\\", "", Parts),
    atomics_to_string(Parts, "/", Normalized).

% ============================================================================
% Definition Extraction using DCGs
% ============================================================================

%! extract_definitions(+SymbolsFile, -Definitions, +Options) is det.
%
% Extract function and type definitions from symbols file using DCGs.
%
% @param SymbolsFile Path to cscope_symbols.txt
% @param Definitions List of def(File, Symbol, Line, Context) terms
% @param Options Option list [debug(Level)]

extract_definitions(SymbolsFile, Definitions, Options) :-
    option(debug(DebugLevel), Options, 0),

    (DebugLevel > 0 ->
        format('  Parsing symbols file: ~w~n', [SymbolsFile])
    ; true),

    read_file_to_string(SymbolsFile, Content, []),
    split_string(Content, "\n", "\r", Lines),

    extract_defs_from_lines(Lines, [], Definitions, DebugLevel).

%! extract_defs_from_lines(+Lines, +Seen, -Defs, +DebugLevel) is det.
%
% Process symbol lines and extract definitions.

extract_defs_from_lines([], _Seen, [], _Debug).
extract_defs_from_lines([Line|Lines], Seen, Defs, Debug) :-
    (   Line == ""
    ->  extract_defs_from_lines(Lines, Seen, Defs, Debug)
    ;   (   split_string(Line, "\t", "", Parts),
            Parts = [File, Scope, LineNum, Context]
        ->  (   Scope \= "<global>",
                \+ member(Scope, Seen)
            ->  % Function definition
                string_codes(Context, Codes),
                (Debug > 1 -> format('  Checking function: ~s in ~s~n', [Scope, Context]) ; true),
                (   member(40, Codes)  % Check for '(' character
                ->  (Debug > 0 -> format('  Found function def: ~s~n', [Scope]) ; true),
                    Defs = [def(File, Scope, LineNum, Context)|RestDefs],
                    extract_defs_from_lines(Lines, [Scope|Seen], RestDefs, Debug)
                ;   (Debug > 1 -> format('  No paren in: ~s~n', [Context]) ; true),
                    extract_defs_from_lines(Lines, Seen, Defs, Debug)
                )
            ;   Scope = "<global>"
            ->  % Global definition (struct, typedef, enum, macro)
                string_codes(Context, Codes),
                (   phrase(global_definition(Symbol, Kind), Codes),
                    Kind \= function
                ->  (Debug > 1 -> format('  Found ~w: ~s~n', [Kind, Symbol]) ; true),
                    Defs = [def(File, Symbol, LineNum, Context)|RestDefs],
                    extract_defs_from_lines(Lines, Seen, RestDefs, Debug)
                ;   extract_defs_from_lines(Lines, Seen, Defs, Debug)
                )
            ;   extract_defs_from_lines(Lines, Seen, Defs, Debug)
            )
        ;   % Parse failed - skip line
            (Debug > 1 -> format('  Warning: failed to parse line: ~s~n', [Line]) ; true),
            extract_defs_from_lines(Lines, Seen, Defs, Debug)
        )
    ).

% ============================================================================
% DCG Rules for Parsing C Definitions
% ============================================================================

%! has_function_call// is semidet.
%
% DCG to detect if line contains a function call (has parentheses).

has_function_call([40|_], [40|_]) :- !.  % Found '('
has_function_call([_|Rest], Remainder) :-
    has_function_call(Rest, Remainder).
has_function_call([], []) :- fail.  % Reached end without finding '('

%! test_has_paren(+String) is semidet.
%
% Test predicate to check if DCG works.

test_has_paren(String) :-
    string_codes(String, Codes),
    format('Codes: ~w~n', [Codes]),
    (phrase(has_function_call, Codes) -> format('Match!~n') ; format('No match~n')),
    phrase(has_function_call, Codes).

%! global_definition(-Symbol, -Kind)// is semidet.
%
% DCG to extract symbol and kind from global definition context.

% Macro definition: #define SYMBOL
global_definition(Symbol, macro) -->
    whites,
    "#define",
    whites,
    c_identifier(Symbol),
    !.

% Typedef: typedef struct NAME { or typedef ... NAME;
global_definition(Symbol, typedef) -->
    whites,
    "typedef",
    whites,
    (   "struct" -> whites ; "union" -> whites ; "enum" -> whites ; ""),
    whites,
    c_identifier(Symbol),
    whites,
    ("{" ; ";"),
    !.

% Struct: struct NAME {
global_definition(Symbol, struct) -->
    whites,
    "struct",
    whites,
    c_identifier(Symbol),
    whites,
    "{",
    !.

% Union: union NAME {
global_definition(Symbol, union) -->
    whites,
    "union",
    whites,
    c_identifier(Symbol),
    whites,
    "{",
    !.

% Enum: enum NAME {
global_definition(Symbol, enum) -->
    whites,
    "enum",
    whites,
    c_identifier(Symbol),
    whites,
    "{",
    !.

%! c_identifier(-Identifier)// is det.
%
% DCG to parse a C identifier.

c_identifier(Identifier) -->
    [First],
    { code_type(First, alpha) ; First = 0'_ },
    c_identifier_rest(Rest),
    { atom_codes(Identifier, [First|Rest]) }.

c_identifier_rest([C|Rest]) -->
    [C],
    { code_type(C, alnum) ; C = 0'_ },
    !,
    c_identifier_rest(Rest).
c_identifier_rest([]) --> [].

% ============================================================================
% Function Call Extraction
% ============================================================================

%! extract_function_calls(+SymbolsFile, -Calls, +Options) is det.
%
% Extract function call relationships from symbols file.
%
% @param SymbolsFile Path to cscope_symbols.txt
% @param Calls List of call(File, Caller, Line, Context, Callee) terms

extract_function_calls(SymbolsFile, Calls, Options) :-
    option(debug(DebugLevel), Options, 0),
    option(cscope_dir(SrcDir), Options),

    % First, extract function names from symbols file
    read_file_to_string(SymbolsFile, Content, []),
    split_string(Content, "\n", "\r", Lines),
    extract_function_names(Lines, [], Functions),

    (DebugLevel > 0 ->
        length(Functions, FuncCount),
        format('  Found ~w functions to query~n', [FuncCount])
    ; true),

    (DebugLevel > 1 ->
        format('  Cscope database directory: ~w~n', [SrcDir])
    ; true),

    % Query each function
    maplist(query_function_calls(SrcDir, DebugLevel), Functions, CallLists),
    append(CallLists, Calls).

%! extract_function_names(+Lines, +Acc, -Functions) is det.
%
% Extract unique function names from symbol lines.

extract_function_names([], Funcs, Funcs).
extract_function_names([Line|Lines], Acc, Funcs) :-
    (   Line == ""
    ->  extract_function_names(Lines, Acc, Funcs)
    ;   split_string(Line, "\t", "", [_File, Scope, _LineNum, _Context]),
        (   Scope \= "<global>",
            \+ member(Scope, Acc)
        ->  extract_function_names(Lines, [Scope|Acc], Funcs)
        ;   extract_function_names(Lines, Acc, Funcs)
        )
    ).

%! query_function_calls(+SrcDir, +Debug, +Function, -Calls) is det.
%
% Query cscope for calls made by Function.

query_function_calls(SrcDir, _Debug, Function, Calls) :-
    catch(
        (
            process_create(path(cscope), ['-d', '-L', '-2', Function],
                           [cwd(SrcDir), stdout(pipe(In)), stderr(null)]),
            read_string(In, _, Output),
            close(In),
            split_string(Output, "\n", "\r ", Lines),
            maplist(parse_call_line(Function), Lines, CallsWithFail),
            exclude(=(fail), CallsWithFail, Calls)
        ),
        _Error,
        (
            % On error, return empty call list
            % format('  Warning: cscope query failed for ~w: ~w~n', [Function, _Error]),
            Calls = []
        )
    ).

%! parse_call_line(+Caller, +Line, -Call) is det.
%
% Parse a cscope call line into call(File, Caller, Line, Context, Callee).

parse_call_line(_Caller, "", fail) :- !.
parse_call_line(Caller, Line, call(NormFile, Caller, LineNum, Context, Callee)) :-
    split_string(Line, " ", "", [File, Callee, LineNum | ContextParts]),
    !,
    atomics_to_string(ContextParts, " ", Context),
    normalize_path(File, NormFile).
parse_call_line(_, _, fail).

% ============================================================================
% File Writing
% ============================================================================

%! write_definitions(+File, +Defs) is det.
%
% Write definitions to file in tab-separated format.

write_definitions(File, Defs) :-
    open(File, write, Out),
    forall(
        member(def(F, S, L, C), Defs),
        format(Out, '~s\t~s\t~s\t~s~n', [F, S, L, C])
    ),
    close(Out).

%! write_calls(+File, +Calls) is det.
%
% Write calls to file in tab-separated format.

write_calls(File, Calls) :-
    open(File, write, Out),
    forall(
        member(call(F, Caller, L, C, _Callee), Calls),
        format(Out, '~s\t~s\t~s\t~s~n', [F, Caller, L, C])
    ),
    close(Out).
