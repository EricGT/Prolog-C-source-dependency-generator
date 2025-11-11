:- module(test_cscope_import_dcg, []).

/** <module> DCG Parser Tests for cscope_import

Unit tests for the public DCG parser interface in cscope_import.pl.

Run tests with:
  ?- run_tests.
  ?- run_tests(parse_def_line_dcg).
  ?- run_tests(parse_call_line_dcg).
  ?- run_tests(parse_symbol_line_dcg).
*/

:- use_module('../../src/prolog/cscope_import').
:- use_module(library(plunit)).

% ============================================================
% ===============  PARSE_DEF_LINE_DCG TESTS  =================
% ============================================================

:- begin_tests(parse_def_line_dcg).

test(parse_def_function, [(File == 'test.c'), (Symbol == main), (Kind == function)]) :-
    string_codes("test.c\tmain\t10\tint main(void) {\n", Codes),
    parse_def_line_dcg(Codes, File, Symbol, _Line, _Context, Kind, 1).

test(parse_def_struct, [Kind == struct]) :-
    string_codes("types.h\tMyStruct\t5\tstruct MyStruct {\n", Codes),
    parse_def_line_dcg(Codes, _File, _Symbol, _Line, _Context, Kind, 1).

test(parse_def_typedef, [Kind == typedef]) :-
    string_codes("defs.h\tsize_t\t42\ttypedef unsigned long size_t;\n", Codes),
    parse_def_line_dcg(Codes, _File, _Symbol, _Line, _Context, Kind, 1).

test(parse_def_macro, [Kind == macro]) :-
    string_codes("config.h\tMAX_SIZE\t1\t#define MAX_SIZE 1024\n", Codes),
    parse_def_line_dcg(Codes, _File, _Symbol, _Line, _Context, Kind, 1).

test(parse_def_enum, [Kind == enum]) :-
    string_codes("enums.h\tStatus\t7\tenum Status { OK, ERR };\n", Codes),
    parse_def_line_dcg(Codes, _File, _Symbol, _Line, _Context, Kind, 1).

test(parse_def_unknown_kind, [Kind == unknown]) :-
    string_codes("misc.c\tvar\t3\tint var;\n", Codes),
    parse_def_line_dcg(Codes, _File, _Symbol, _Line, _Context, Kind, 1).

test(parse_def_path_normalization, [File == 'c:/users/code/file.c']) :-
    string_codes("C:\\Users\\Code\\File.c\tfunc\t10\tvoid func(void) {\n", Codes),
    parse_def_line_dcg(Codes, File, _Symbol, _Line, _Context, _Kind, 1).

test(parse_def_complete_data, [
    (File == 'main.c'),
    (Symbol == main),
    (Line == 42),
    (Context == "int main(int argc, char **argv) {"),
    (Kind == function)
]) :-
    string_codes("main.c\tmain\t42\tint main(int argc, char **argv) {\n", Codes),
    parse_def_line_dcg(Codes, File, Symbol, Line, Context, Kind, 1).

test(parse_def_invalid_missing_tab, [fail]) :-
    string_codes("file.c main 10 context\n", Codes),
    parse_def_line_dcg(Codes, _File, _Symbol, _Line, _Context, _Kind, 1).

test(parse_def_invalid_non_numeric_line, [fail]) :-
    string_codes("file.c\tmain\tabc\tcontext\n", Codes),
    parse_def_line_dcg(Codes, _File, _Symbol, _Line, _Context, _Kind, 1).

test(parse_def_underscore_symbol, [Symbol == _internal_func]) :-
    string_codes("util.c\t_internal_func\t50\tstatic void _internal_func(void) {\n", Codes),
    parse_def_line_dcg(Codes, _File, Symbol, _Line, _Context, _Kind, 1).

test(parse_def_complex_context, [Context == "static inline int complex_func(struct data *ptr, int flags) {"]) :-
    string_codes("complex.c\tcomplex_func\t200\tstatic inline int complex_func(struct data *ptr, int flags) {\n", Codes),
    parse_def_line_dcg(Codes, _File, _Symbol, _Line, Context, _Kind, 1).

test(parse_def_large_line_num, [Line == 999999]) :-
    string_codes("big.c\tfunc\t999999\tvoid func(void) {\n", Codes),
    parse_def_line_dcg(Codes, _File, _Symbol, Line, _Context, _Kind, 1).

test(parse_def_no_newline, [Symbol == foo]) :-
    string_codes("test.c\tfoo\t7\tvoid foo(void);", Codes),
    parse_def_line_dcg(Codes, _File, Symbol, _Line, _Context, _Kind, 1).

:- end_tests(parse_def_line_dcg).

% ============================================================
% ===============  PARSE_CALL_LINE_DCG TESTS  ================
% ============================================================

:- begin_tests(parse_call_line_dcg).

test(parse_call_simple, [(File == 'main.c'), (Caller == main), (Callee == printf)]) :-
    string_codes("main.c\tmain\t15\tprintf(\"Hello\");\n", Codes),
    parse_call_line_dcg(Codes, File, Caller, _Line, _Context, Callee, 1).

test(parse_call_with_args, [Callee == validate]) :-
    string_codes("process.c\tprocess\t42\tif (validate(data)) {\n", Codes),
    parse_call_line_dcg(Codes, _File, _Caller, _Line, _Context, Callee, 1).

test(parse_call_underscore_name, [Callee == _helper_func]) :-
    string_codes("util.c\tpublic_func\t20\t_helper_func(x, y);\n", Codes),
    parse_call_line_dcg(Codes, _File, _Caller, _Line, _Context, Callee, 1).

test(parse_call_no_match, [Callee == unknown]) :-
    string_codes("weird.c\tfunc\t5\t// just a comment\n", Codes),
    parse_call_line_dcg(Codes, _File, _Caller, _Line, _Context, Callee, 1).

test(parse_call_path_normalization, [File == '/usr/src/file.c']) :-
    string_codes("/usr/src/file.c\tfunc\t100\tcall();\n", Codes),
    parse_call_line_dcg(Codes, File, _Caller, _Line, _Context, _Callee, 1).

test(parse_call_complete_data, [
    (File == 'process.c'),
    (Caller == handler),
    (Line == 100),
    (Context == "callback(data);"),
    (Callee == callback)
]) :-
    string_codes("process.c\thandler\t100\tcallback(data);\n", Codes),
    parse_call_line_dcg(Codes, File, Caller, Line, Context, Callee, 1).

test(parse_call_nested_call, [(Caller == process), (Line == 42), (Context == "if (validate(data)) {")]) :-
    string_codes("process.c\tprocess\t42\tif (validate(data)) {\n", Codes),
    parse_call_line_dcg(Codes, _File, Caller, Line, Context, _Callee, 1).

test(parse_call_pointer_deref, [Context == "(*callback)(data);"]) :-
    string_codes("callbacks.c\thandler\t30\t(*callback)(data);\n", Codes),
    parse_call_line_dcg(Codes, _File, _Caller, _Line, Context, _Callee, 1).

test(parse_call_macro_call, [Context == "ASSERT(ptr != NULL);"]) :-
    string_codes("test.c\ttest_func\t10\tASSERT(ptr != NULL);\n", Codes),
    parse_call_line_dcg(Codes, _File, _Caller, _Line, Context, _Callee, 1).

test(parse_call_invalid_format, [fail]) :-
    string_codes("invalid line format\n", Codes),
    parse_call_line_dcg(Codes, _File, _Caller, _Line, _Context, _Callee, 1).

:- end_tests(parse_call_line_dcg).

% ============================================================
% ===============  PARSE_SYMBOL_LINE_DCG TESTS  ==============
% ============================================================

:- begin_tests(parse_symbol_line_dcg).

test(parse_symbol_simple, [(File == 'file.c'), (Symbol == var), (Line == 10)]) :-
    string_codes("file.c\tvar\t10\tint var = 0;\n", Codes),
    parse_symbol_line_dcg(Codes, File, Symbol, Line, _Context, 1).

test(parse_symbol_global, [Symbol == global_counter]) :-
    string_codes("globals.c\tglobal_counter\t5\tstatic int global_counter = 0;\n", Codes),
    parse_symbol_line_dcg(Codes, _File, Symbol, _Line, _Context, 1).

test(parse_symbol_path_norm, [File == 'c:/windows/system32/file.c']) :-
    string_codes("C:\\Windows\\System32\\File.c\tsymbol\t1\tcontext\n", Codes),
    parse_symbol_line_dcg(Codes, File, _Symbol, _Line, _Context, 1).

test(parse_symbol_complete_data, [
    (File == 'data.c'),
    (Symbol == array),
    (Line == 25),
    (Context == "int array[MAX_SIZE];")
]) :-
    string_codes("data.c\tarray\t25\tint array[MAX_SIZE];\n", Codes),
    parse_symbol_line_dcg(Codes, File, Symbol, Line, Context, 1).

test(parse_symbol_in_struct, [Context == "struct point { int x; int y; };"]) :-
    string_codes("types.h\tpoint\t20\tstruct point { int x; int y; };\n", Codes),
    parse_symbol_line_dcg(Codes, _File, _Symbol, _Line, Context, 1).

test(parse_symbol_pointer_decl, [Context == "void *ptr = NULL;"]) :-
    string_codes("memory.c\tptr\t50\tvoid *ptr = NULL;\n", Codes),
    parse_symbol_line_dcg(Codes, _File, _Symbol, _Line, Context, 1).

test(parse_symbol_invalid, [fail]) :-
    string_codes("bad format line\n", Codes),
    parse_symbol_line_dcg(Codes, _File, _Symbol, _Line, _Context, 1).

:- end_tests(parse_symbol_line_dcg).

% ============================================================
% ===============  INTEGRATION TESTS  ========================
% ============================================================

:- begin_tests(integration).

test(real_world_def_line, [
    (File == 'c:/users/eric/projects/prolog-c-source-dependency-generator/fixtures/basic-deps/src/level1_utils.c'),
    (Symbol == min),
    (Line == 3),
    (Kind == function)
]) :-
    % Actual line from cscope output
    string_codes("c:/users/eric/projects/prolog-c-source-dependency-generator/fixtures/basic-deps/src/level1_utils.c\tmin\t3\tint min(int a, int b) {\n", Codes),
    parse_def_line_dcg(Codes, File, Symbol, Line, _Context, Kind, 1).

test(real_world_call_line, [(Caller == parse_token), (Callee == validate_length)]) :-
    % Actual line from cscope callees output
    string_codes("c:/users/eric/projects/prolog-c-source-dependency-generator/fixtures/basic-deps/src/level3_parser.c\tparse_token\t12\tif (!validate_length(input, 256)) {\n", Codes),
    parse_call_line_dcg(Codes, _File, Caller, _Line, _Context, Callee, 1).

test(multiple_lines_def, [Results = [item('file1.c', func1, 10), item('file2.c', func2, 20), item('file3.c', func3, 30)]]) :-
    Lines = [
        "file1.c\tfunc1\t10\tint func1(void) {\n",
        "file2.c\tfunc2\t20\tvoid func2(int x) {\n",
        "file3.c\tfunc3\t30\tstatic void func3(void) {\n"
    ],
    maplist(string_codes, Lines, CodesList),
    maplist([Codes, Result]>>(
        parse_def_line_dcg(Codes, File, Symbol, Line, _Context, _Kind, 1),
        Result = item(File, Symbol, Line)
    ), CodesList, Results).

test(mixed_line_types, [Results = [def(func1), call(func2), symbol(var1)]]) :-
    % Test parsing different line types
    string_codes("file.c\tfunc1\t1\tint func1(void) {\n", DefCodes),
    string_codes("file.c\tfunc2\t2\tcall();\n", CallCodes),
    string_codes("file.c\tvar1\t3\tint var1;\n", SymbolCodes),
    parse_def_line_dcg(DefCodes, _F1, S1, _L1, _C1, _K1, 1),
    parse_call_line_dcg(CallCodes, _F2, S2, _L2, _C2, _Ce2, 1),
    parse_symbol_line_dcg(SymbolCodes, _F3, S3, _L3, _C3, 1),
    Results = [def(S1), call(S2), symbol(S3)].

:- end_tests(integration).

% ============================================================
% ===============  EDGE CASE TESTS  ==========================
% ============================================================

:- begin_tests(edge_cases).

test(zero_line_number, [Line == 0]) :-
    string_codes("file.c\tsym\t0\tcontext\n", Codes),
    parse_def_line_dcg(Codes, _File, _Symbol, Line, _Context, _Kind, 1).

test(very_long_context) :-
    % Generate a very long context line (200 characters)
    length(LongList, 200),
    maplist(=(0'x), LongList),
    append("file.c\tsym\t1\t", LongList, Line1),
    append(Line1, "\n", FullLine),
    parse_def_line_dcg(FullLine, _File, _Symbol, _Line, Context, _Kind, 1),
    string_length(Context, Len),
    Len == 200.

test(unicode_in_path, [File == "файл.c"]) :-
    % Cyrillic filename
    string_codes("файл.c\tsymbol\t5\tcontext\n", Codes),
    parse_symbol_line_dcg(Codes, File, _Symbol, _Line, _Context, 1).

test(special_chars_in_context, [Context == "<>&\"'!@#$%^&*()"]) :-
    string_codes("file.c\tsym\t1\t<>&\"'!@#$%^&*()\n", Codes),
    parse_def_line_dcg(Codes, _File, _Symbol, _Line, Context, _Kind, 1).

test(empty_symbol_field, [Symbol == ""]) :-
    % Empty symbol name parses but results in empty string
    string_codes("file.c\t\t10\tcontext\n", Codes),
    parse_symbol_line_dcg(Codes, _File, Symbol, _Line, _Context, 1).

test(windows_crlf, [Symbol == func]) :-
    string_codes("file.c\tfunc\t1\tcontext\r\n", Codes),
    parse_def_line_dcg(Codes, _File, Symbol, _Line, _Context, _Kind, 1).

test(mac_cr, [Symbol == func]) :-
    string_codes("file.c\tfunc\t1\tcontext\r", Codes),
    parse_def_line_dcg(Codes, _File, Symbol, _Line, _Context, _Kind, 1).

test(unix_lf, [Symbol == func]) :-
    string_codes("file.c\tfunc\t1\tcontext\n", Codes),
    parse_def_line_dcg(Codes, _File, Symbol, _Line, _Context, _Kind, 1).

test(no_line_ending, [Symbol == func]) :-
    string_codes("file.c\tfunc\t1\tcontext", Codes),
    parse_def_line_dcg(Codes, _File, Symbol, _Line, _Context, _Kind, 1).

:- end_tests(edge_cases).
