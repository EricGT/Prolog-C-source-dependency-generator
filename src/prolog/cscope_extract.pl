:- module(cscope_extract, [
    generate_cscope_data/2,    % generate_cscope_data(+SrcDir, +Options)
    extract_definitions/3,      % extract_definitions(+SymbolsFile, -Defs, +Options)
    extract_function_calls/3,   % extract_function_calls(+SymbolsFile, -Calls, +Options)
    % Exported DCG rules for testing
    global_definition//2,       % global_definition(-Symbol, -Kind)
    c_identifier//1,            % c_identifier(-Identifier)
    c_identifier_rest//1,       % c_identifier_rest(-Rest)
    symbol_line//4,             % symbol_line(-Def, +Seen0, -Seen, +Debug)
    symbol_file_lines//3,       % symbol_file_lines(-Defs, +Seen0, +Debug)
    normalize_path_codes//1,    % normalize_path_codes(-NormCodes)
    field_until_tab//1,         % field_until_tab(-Codes)
    field_until_space//1,       % field_until_space(-Codes)
    field_until_newline//1,     % field_until_newline(-Codes)
    newline//0,                 % newline
    'whites*'//0,               % 'whites*'
    'whites+'//0,               % 'whites+'
    def_line_fields//4,         % def_line_fields(-File, -Scope, -Line, -ContextCodes)
    function_names_file//2      % function_names_file(-Functions, +Acc0)
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
:- use_module(library(error)).

%! generate_cscope_data(+SrcDir, +Options) is det.
%
% Generate all cscope data files from source directory.
%
% Accepts SrcDir in multiple formats:
%   - Unix-style: /home/user/project or ~/project
%   - Windows-style: C:\Users\Project or C:/Users/Project
%   - Git Bash-style: /c/Users/Project (converted to C:/Users/Project on Windows)
%   - Relative: ./project or ../other (resolved against root or cwd)
%
% Options:
%   - root(Path): Project root directory (default: '.')
%   - debug(Level): Debug level 0-3 (default: 0)
%   - quiet(Bool): Suppress output (default: false)
%
% @param SrcDir Source directory containing C source files
% @param Options Option list

generate_cscope_data(SrcDir, Options) :-
    % Parse options
    option(root(Root), Options, '.'),
    option(debug(DebugLevel), Options, 0),
    option(quiet(Quiet), Options, false),
    !,  % Commit to options - ensure determinism

    % Convert SrcDir to absolute path in native OS format
    % Handles Unix/Windows/Git Bash/relative paths
    (   atom(SrcDir)
    ->  atom_string(SrcDir, SrcDirStr)
    ;   SrcDirStr = SrcDir
    ),

    % Detect OS
    (   current_prolog_flag(windows, true)
    ->  IsWindows = true
    ;   IsWindows = false
    ),

    % Convert Git Bash paths on Windows (/c/... -> C:/...)
    (   IsWindows = true,
        string_concat("/", Rest, SrcDirStr),
        string_length(Rest, Len),
        Len >= 2,
        sub_string(Rest, 0, 1, _, DriveLetter),
        char_type(DriveLetter, alpha),
        sub_string(Rest, 1, 1, _, "/")
    ->  upcase_atom(DriveLetter, DriveUpper),
        string_concat("/", RestPath, Rest),
        sub_string(RestPath, 1, _, 0, AfterDrive),
        atomics_to_string([DriveUpper, ":/", AfterDrive], ConvertedPath)
    ;   ConvertedPath = SrcDirStr
    ),

    % Resolve to absolute path
    % Try relative to root first, then cwd
    (   catch(
            (   atom_string(Root, RootStr),
                string_concat(RootStr, "/", RootWithSlash),
                string_concat(RootWithSlash, ConvertedPath, TestPath),
                atom_string(TestPathAtom, TestPath),
                exists_directory(TestPathAtom),
                absolute_file_name(TestPathAtom, AbsSrcDir)
            ),
            _,
            fail
        )
    ->  true
    ;   catch(
            (   atom_string(ConvertedPathAtom, ConvertedPath),
                absolute_file_name(ConvertedPathAtom, AbsSrcDir)
            ),
            _,
            (   format(user_error, 'Error: Cannot resolve path: ~w~n', [SrcDir]),
                fail
            )
        )
    ),

    % Validate directory exists
    (   exists_directory(AbsSrcDir)
    ->  true
    ;   format(user_error, 'Error: Directory does not exist: ~w~n', [AbsSrcDir]),
        fail
    ),

    % Set up directories
    atom_concat(Root, '/data/extracted', ExtractedDir),
    atom_concat(Root, '/logs', LogDir),

    % Create directories
    make_directory_path(ExtractedDir),
    make_directory_path(LogDir),

    % Step 1: Build cscope database
    (Quiet == false -> format('[1/6] Building cscope database...~n', []) ; true),
    build_cscope_database(AbsSrcDir, DebugLevel),

    % Step 2-5: Extract data
    (Quiet == false -> format('[2/6] Extracting symbols...~n', []) ; true),
    extract_symbols(AbsSrcDir, ExtractedDir, SymbolCount, DebugLevel),

    (Quiet == false -> format('[3/6] Extracting callers...~n', []) ; true),
    extract_callers(AbsSrcDir, ExtractedDir, CallerCount, DebugLevel),

    (Quiet == false -> format('[4/6] Extracting includes...~n', []) ; true),
    extract_includes(AbsSrcDir, ExtractedDir, IncludeCount, DebugLevel),

    (Quiet == false -> format('[5/6] Extracting definitions...~n', []) ; true),
    atom_concat(ExtractedDir, '/cscope_symbols.txt', SymbolsFile),
    extract_definitions(SymbolsFile, Defs, [debug(DebugLevel)]),
    atom_concat(ExtractedDir, '/cscope_definitions.txt', DefsFile),
    write_definitions(DefsFile, Defs),
    length(Defs, DefCount),

    (Quiet == false -> format('[6/6] Extracting function calls...~n', []) ; true),
    extract_function_calls(SymbolsFile, Calls, [debug(DebugLevel), cscope_dir(AbsSrcDir)]),
    atom_concat(ExtractedDir, '/cscope_callees.txt', CallsFile),
    write_calls(CallsFile, Calls),
    length(Calls, CallCount),

    % Summary
    (Quiet == true ->
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
% Uses single-pass DCG parsing for efficiency.

read_and_reformat_lines(In, Out, _Acc, Total) :-
    read_stream_to_codes(In, Codes),
    phrase(cscope_stream_lines(Lines), Codes),
    write_reformatted_lines(Out, Lines, 0, Total).

%! reformat_cscope_line(+Line, -Reformatted) is det.
%
% Reformat space-separated cscope output to tab-separated.
% Uses single-pass DCG to parse fields and reformats with tabs.

reformat_cscope_line(Line, Reformatted) :-
    string_codes(Line, Codes),
    phrase(parse_cscope_line(File, Symbol, LineNum, Context), Codes),
    normalize_path(File, NormFile),
    atomics_to_string([NormFile, Symbol, LineNum, Context], "\t", Reformatted).

%! cscope_stream_lines(-Lines)// is det.
%
% Parse entire cscope stream and produce list of reformatted lines.

cscope_stream_lines([Line|Lines]) -->
    parse_cscope_line(File, Symbol, LineNum, Context),
    newline,
    !,
    {
        normalize_path(File, NormFile),
        atomics_to_string([NormFile, Symbol, LineNum, Context], "\t", Line)
    },
    cscope_stream_lines(Lines).
cscope_stream_lines([]) -->
    [].

%! parse_cscope_line(-File, -Symbol, -LineNum, -Context)// is det.
%
% Parse space-separated cscope line: File Symbol LineNum Context

parse_cscope_line(File, Symbol, LineNum, Context) -->
    field_until_space(FileCodes),
    { string_codes(File, FileCodes) },
    space,
    field_until_space(SymbolCodes),
    { string_codes(Symbol, SymbolCodes) },
    space,
    field_until_space(LineNumCodes),
    { string_codes(LineNum, LineNumCodes) },
    space,
    field_until_newline(ContextCodes),
    { string_codes(Context, ContextCodes) }.

%! write_reformatted_lines(+Out, +Lines, +Acc, -Total) is det.
%
% Write reformatted lines to output stream.

write_reformatted_lines(_, [], Count, Count) :- !.
write_reformatted_lines(Out, [Line|Lines], Acc, Total) :-
    format(Out, '~s~n', [Line]),
    succ(Acc, Acc1),
    write_reformatted_lines(Out, Lines, Acc1, Total).

%! normalize_path(+Path, -Normalized) is det.
%
% Normalize file path for Prolog (lowercase, forward slashes).
% Uses single-pass DCG to lowercase and replace backslashes with forward slashes.

normalize_path(Path, Normalized) :-
    string_codes(Path, PathCodes),
    phrase(normalize_path_codes(NormCodes), PathCodes),
    string_codes(Normalized, NormCodes).

%! normalize_path_codes(-NormalizedCodes)// is det.
%
% DCG to normalize path: lowercase and replace backslashes with forward slashes.

% Replace backslash with forward slash
normalize_path_codes([0'/|Cs]) -->
    [0'\\],
    !,
    normalize_path_codes(Cs).

% Convert uppercase to lowercase
normalize_path_codes([Lower|Cs]) -->
    [C],
    { code_type(C, upper), to_lower(C, Lower) },
    !,
    normalize_path_codes(Cs).

% Keep character as-is
normalize_path_codes([C|Cs]) -->
    [C],
    !,
    normalize_path_codes(Cs).

% Base case: end of input
normalize_path_codes([]) -->
    [].

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

    catch(
        phrase_from_file(symbol_file_lines(Definitions, [], DebugLevel), SymbolsFile),
        Error,
        handle_parse_file_error(Error, SymbolsFile, Definitions)
    ).

%! handle_parse_file_error(+Error, +Filename, -Result) is det.
%
% Handle file parsing errors gracefully.

handle_parse_file_error(error(existence_error(source_sink, Filename), _), Filename, []) :-
    !,
    format(user_error, 'Error: File not found: ~w~n', [Filename]).

handle_parse_file_error(error(permission_error(open, source_sink, Filename), _), Filename, []) :-
    !,
    format(user_error, 'Error: Permission denied: ~w~n', [Filename]).

handle_parse_file_error(Error, Filename, []) :-
    format(user_error, 'Error parsing file ~w: ~w~n', [Filename, Error]).

%! symbol_file_lines(-Defs, +Seen0, +Debug)// is det.
%
% Parse entire symbol file directly using DCG.
% Uses phrase_from_file for single-pass file parsing.

% When we get a proper definition, add it to the list
symbol_file_lines([Def|RestDefs], Seen0, Debug) -->
    symbol_line(Def, Seen0, Seen1, Debug),
    { Def = def(_, _, _, _) },
    !,
    symbol_file_lines(RestDefs, Seen1, Debug).

% When we get 'none', skip it but continue with same Seen
symbol_file_lines(Defs, Seen0, Debug) -->
    symbol_line(none, Seen0, Seen0, Debug),
    !,
    symbol_file_lines(Defs, Seen0, Debug).

% Base case: end of input
symbol_file_lines([], _, _) -->
    [].

%! symbol_line(-Def, +Seen0, -Seen, +Debug)// is det.
%
% Parse single symbol line, extracting definition if found.

% Empty line - skip
symbol_line(none, Seen, Seen, _Debug) -->
    'whites*',
    newline,
    !.

% Function definition: not global, not seen, has paren
symbol_line(def(File, Scope, LineNum, Context), Seen0, [Scope|Seen0], Debug) -->
    def_line_fields(File, Scope, LineNum, ContextCodes),
    {
        Scope \= "<global>",
        \+ memberchk(Scope, Seen0),
        memberchk(40, ContextCodes),  % Has '(' character
        string_codes(Context, ContextCodes),
        (Debug > 1 -> format('  Checking function: ~s in ~s~n', [Scope, Context]) ; true),
        (Debug > 0 -> format('  Found function def: ~s~n', [Scope]) ; true)
    },
    newline,
    !.

% Not a function (no paren): not global, not seen, no paren
symbol_line(none, Seen0, Seen0, Debug) -->
    def_line_fields(_File, Scope, _LineNum, ContextCodes),
    {
        Scope \= "<global>",
        \+ memberchk(Scope, Seen0),
        \+ memberchk(40, ContextCodes),  % No '(' character
        (Debug > 1 ->
            string_codes(Context, ContextCodes),
            format('  No paren in: ~s~n', [Context])
        ; true)
    },
    newline,
    !.

% Global definition: struct, typedef, enum, macro
symbol_line(def(File, Symbol, LineNum, Context), Seen0, Seen0, Debug) -->
    def_line_fields(File, "<global>", LineNum, ContextCodes),
    {
        phrase(global_definition(Symbol, Kind), ContextCodes, _Rest),
        Kind \= function,
        string_codes(Context, ContextCodes),
        (Debug > 1 -> format('  Found ~w: ~s~n', [Kind, Symbol]) ; true)
    },
    newline,
    !.

% Global scope but not a recognized definition
symbol_line(none, Seen0, Seen0, _Debug) -->
    def_line_fields(_File, "<global>", _LineNum, _ContextCodes),
    newline,
    !.

% Already seen function - skip
symbol_line(none, Seen0, Seen0, _Debug) -->
    def_line_fields(_File, Scope, _LineNum, _ContextCodes),
    {
        Scope \= "<global>",
        memberchk(Scope, Seen0)
    },
    newline,
    !.

% Failed parse - skip line
symbol_line(none, Seen, Seen, Debug) -->
    field_until_newline(LineCodes),
    newline,
    {
        (Debug > 1 ->
            string_codes(Line, LineCodes),
            format('  Warning: failed to parse line: ~s~n', [Line])
        ; true)
    }.

%! def_line_fields(-File, -Scope, -LineNum, -ContextCodes)// is det.
%
% Parse tab-separated definition line: File<tab>Scope<tab>LineNum<tab>Context
% Returns ContextCodes as codes for further DCG parsing.

def_line_fields(File, Scope, LineNum, ContextCodes) -->
    field_until_tab(FileCodes),
    { string_codes(File, FileCodes) },
    tab,
    field_until_tab(ScopeCodes),
    { string_codes(Scope, ScopeCodes) },
    tab,
    field_until_tab(LineNumCodes),
    { string_codes(LineNum, LineNumCodes) },
    tab,
    field_until_newline(ContextCodes).

% ============================================================================
% DCG Rules for Parsing C Definitions
% ============================================================================

%! global_definition(-Symbol, -Kind)// is semidet.
%
% DCG to extract symbol and kind from global definition context.
% Parses C global definitions: macros, typedefs, structs, unions, enums.

% Macro definition: #define SYMBOL
global_definition(Symbol, macro) -->
    'whites*',
    `#define`,
    'whites+',
    c_identifier(Symbol),
    !.

% Typedef: typedef struct NAME { or typedef ... NAME;
global_definition(Symbol, typedef) -->
    'whites*',
    `typedef`,
    'whites+',
    'typedef_keyword?',
    'whites*',
    c_identifier(Symbol),
    'whites*',
    (   `{`
    ;   `;`
    ),
    !.

% Struct: struct NAME {
global_definition(Symbol, struct) -->
    'whites*',
    `struct`,
    'whites+',
    c_identifier(Symbol),
    'whites*',
    `{`,
    !.

% Union: union NAME {
global_definition(Symbol, union) -->
    'whites*',
    `union`,
    'whites+',
    c_identifier(Symbol),
    'whites*',
    `{`,
    !.

% Enum: enum NAME {
global_definition(Symbol, enum) -->
    'whites*',
    `enum`,
    'whites+',
    c_identifier(Symbol),
    'whites*',
    `{`,
    !.

%! 'typedef_keyword?'// is det.
%
% Optional typedef keyword (struct, union, or enum).

'typedef_keyword?' -->
    `struct`,
    'whites+',
    !.
'typedef_keyword?' -->
    `union`,
    'whites+',
    !.
'typedef_keyword?' -->
    `enum`,
    'whites+',
    !.
'typedef_keyword?' -->
    [].

%! c_identifier(-Identifier)// is det.
%
% Parse a C-style identifier.
% C identifiers start with letter or underscore, followed by alphanumerics or underscores.

c_identifier(Identifier) -->
    [First],
    {
        (   code_type(First, alpha)
        ->  true
        ;   First == 0'_
        )
    },
    c_identifier_rest(Rest),
    { atom_codes(Identifier, [First|Rest]) }.

%! c_identifier_rest(-Rest)// is det.
%
% Parse the rest of a C identifier (alphanumerics and underscores).

c_identifier_rest([C|Cs]) -->
    [C],
    {
        code_type(C, alnum)
    ;
        C == 0'_
    },
    !,
    c_identifier_rest(Cs).
c_identifier_rest([]) -->
    [].

% ============================================================================
% Common Field Parsing DCG Helpers
% ============================================================================

%! field_until_tab(-FieldCodes)// is det.
%
% Parse characters until tab (code 9).

field_until_tab([C|Cs], [C|In], Out) :-
    C \== 9,
    !,
    field_until_tab(Cs, In, Out).
field_until_tab([], In, In).

%! field_until_space(-FieldCodes)// is det.
%
% Parse characters until space (code 32).

field_until_space([C|Cs], [C|In], Out) :-
    C \== 32,
    !,
    field_until_space(Cs, In, Out).
field_until_space([], In, In).

%! field_until_newline(-FieldCodes)// is det.
%
% Parse characters until newline (LF=10 or CR=13).

field_until_newline([C|Cs], [C|In], Out) :-
    C \== 10,
    C \== 13,
    !,
    field_until_newline(Cs, In, Out).
field_until_newline([], In, In).

%! skip_until_tab// is det.
%
% Skip characters until tab, consuming up to but not including tab.

skip_until_tab -->
    [C],
    { C \== 9 },  % Not tab
    !,
    skip_until_tab.
skip_until_tab -->
    [].

%! skip_until_newline// is det.
%
% Skip characters until newline, consuming up to but not including newline.

skip_until_newline -->
    [C],
    { C \== 10, C \== 13 },  % Not LF or CR
    !,
    skip_until_newline.
skip_until_newline -->
    [].

% ============================================================================
% Character Primitives and Whitespace Helpers
% ============================================================================

%! tab// is det.
%
% Match a single tab character.

tab -->
    [9].  % ASCII tab

%! space// is det.
%
% Match a single space character.

space -->
    [32].  % ASCII space

%! newline// is det.
%
% Match any newline sequence (CRLF, LF, or CR).

newline -->
    [13, 10],  % CR LF
    !.
newline -->
    [10].      % LF
newline -->
    [13].      % CR

%! 'whites*'// is det.
%
% Zero or more whitespace characters.

'whites*' -->
    [C],
    { code_type(C, space) },
    !,
    'whites*'.
'whites*' -->
    [].

%! 'whites+'// is det.
%
% One or more whitespace characters.

'whites+' -->
    [C],
    { code_type(C, space) },
    'whites*'.

%! rest_of_line(-Codes)// is det.
%
% Capture all remaining characters (equivalent to string//1 from dcg/basics).

rest_of_line(Cs, Cs, []).

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

    % First, extract function names from symbols file using phrase_from_file
    catch(
        phrase_from_file(function_names_file(Functions, []), SymbolsFile),
        Error,
        handle_parse_file_error(Error, SymbolsFile, Functions)
    ),

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

%! function_names_file(-Functions, +Acc0)// is det.
%
% Parse entire file to extract unique function names using DCG.
% Uses phrase_from_file for single-pass file parsing.

% Found a new function name - add it to the accumulator
function_names_file(Functions, Acc0) -->
    function_name_line(Scope, Acc0),
    {
        Scope \= none,
        Scope \= "<global>",
        \+ memberchk(Scope, Acc0)
    },
    !,
    function_names_file(Functions, [Scope|Acc0]).

% Found something but it's none, global, or already seen - skip it
function_names_file(Functions, Acc0) -->
    function_name_line(_Scope, Acc0),
    !,
    function_names_file(Functions, Acc0).

% Base case: end of input
function_names_file(Functions, Functions) -->
    [].

%! function_name_line(-Scope, +Acc)// is det.
%
% Parse single line to extract Scope field (2nd field).

% Empty line - skip
function_name_line(none, _Acc) -->
    'whites*',
    newline,
    !.

% Non-empty line - extract scope
function_name_line(Scope, _Acc) -->
    extract_scope_field(Scope),
    newline,
    !.

% Failed parse - skip line
function_name_line(none, _Acc) -->
    skip_until_newline,
    newline.

%! extract_scope_field(-Scope)// is det.
%
% Extract just the Scope field (2nd field) from tab-separated line.
% Format: File<tab>Scope<tab>LineNum<tab>Context

extract_scope_field(Scope) -->
    skip_until_tab,  % Skip File field
    tab,
    field_until_tab(ScopeCodes),
    { string_codes(Scope, ScopeCodes) },
    skip_until_newline.  % Skip rest of line

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
% Uses single-pass DCG to parse space-separated fields.

parse_call_line(_Caller, "", fail) :- !.
parse_call_line(Caller, Line, call(NormFile, Caller, LineNum, Context, Callee)) :-
    string_codes(Line, Codes),
    phrase(call_line_fields(File, Callee, LineNum, Context), Codes),
    !,
    normalize_path(File, NormFile).
parse_call_line(_, _, fail).

%! call_line_fields(-File, -Callee, -LineNum, -Context)// is det.
%
% Parse space-separated call line: File Callee LineNum Context

call_line_fields(File, Callee, LineNum, Context) -->
    field_until_space(FileCodes),
    { string_codes(File, FileCodes) },
    space,
    field_until_space(CalleeCodes),
    { string_codes(Callee, CalleeCodes) },
    space,
    field_until_space(LineNumCodes),
    { string_codes(LineNum, LineNumCodes) },
    space,
    rest_of_line(ContextCodes),
    { string_codes(Context, ContextCodes) }.

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
