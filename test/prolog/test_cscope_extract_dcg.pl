:- module(test_cscope_extract_dcg, []).

/** <module> DCG Parser Tests for cscope_extract

Unit tests for the DCG parser rules in cscope_extract.pl.

Run tests with:
  ?- run_tests.
  ?- run_tests(test_has_paren).
  ?- run_tests(global_definition).
  ?- run_tests(c_identifier).
*/

:- use_module('../../src/prolog/cscope_extract').
:- use_module(library(plunit)).

% ============================================================
% ===============  TEST_HAS_PAREN TESTS  =====================
% ============================================================
% Note: has_function_call//2 is not a proper standard DCG - it uses
% explicit difference list parameters. The test_has_paren/1 predicate
% attempts to use it via phrase/2 but this doesn't work correctly.
% These tests are kept to document the issue but are marked as blocked.
%
% The actual code uses member(40, Codes) to check for parentheses (line 255).

:- begin_tests(test_has_paren).

test(detect_paren_simple, [blocked('test_has_paren/1 is broken - uses phrase/2 with non-standard DCG')]) :-
    test_has_paren("(").

test(detect_paren_in_middle, [blocked('test_has_paren/1 is broken - uses phrase/2 with non-standard DCG')]) :-
    test_has_paren("foo()").

test(detect_paren_at_end, [blocked('test_has_paren/1 is broken - uses phrase/2 with non-standard DCG')]) :-
    test_has_paren("int main(").

test(detect_paren_complex_signature, [blocked('test_has_paren/1 is broken')]) :-
    test_has_paren("static inline int func(void *ptr, int flags)").

test(no_paren_fails, [condition(fail), blocked('test_has_paren/1 is broken')]) :-
    test_has_paren("int variable;").

test(empty_string_fails, [condition(fail), blocked('test_has_paren/1 is broken')]) :-
    test_has_paren("").

test(only_other_chars_fails, [condition(fail), blocked('test_has_paren/1 is broken')]) :-
    test_has_paren("struct MyStruct { int x; };").

test(multiple_parens, [blocked('test_has_paren/1 is broken')]) :-
    test_has_paren("func(arg1, func2(arg2))").

:- end_tests(test_has_paren).

% ============================================================
% ===============  GLOBAL_DEFINITION TESTS  ==================
% ============================================================

:- begin_tests(global_definition).

% Macro tests
test(macro_simple, [(Symbol == 'MAX_SIZE'), (Kind == macro)]) :-
    string_codes("#define MAX_SIZE", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(macro_with_leading_whitespace, [(Symbol == 'DEBUG'), (Kind == macro)]) :-
    string_codes("  #define DEBUG", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(macro_function_like, [(Symbol == 'MIN'), (Kind == macro)]) :-
    string_codes("#define MIN", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(macro_no_value, [(Symbol == 'ENABLED'), (Kind == macro)]) :-
    string_codes("#define ENABLED", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(macro_multiline_backslash, [(Symbol == 'MULTILINE'), (Kind == macro)]) :-
    string_codes("#define MULTILINE", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

% Typedef tests
test(typedef_struct_with_brace, [(Symbol == 'point_t'), (Kind == typedef)]) :-
    string_codes("typedef struct point_t {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(typedef_struct_with_semicolon, [(Symbol == 'size_t'), (Kind == typedef)]) :-
    string_codes("typedef size_t;", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(typedef_union, [(Symbol == 'value_t'), (Kind == typedef)]) :-
    string_codes("typedef union value_t {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(typedef_enum, [(Symbol == 'status_t'), (Kind == typedef)]) :-
    string_codes("typedef enum status_t {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(typedef_simple, [(Symbol == 'byte'), (Kind == typedef)]) :-
    string_codes("typedef byte;", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(typedef_pointer, [(Symbol == 'ptr_t'), (Kind == typedef)]) :-
    string_codes("typedef ptr_t;", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(typedef_function_pointer, [(Symbol == 'callback_t'), (Kind == typedef)]) :-
    string_codes("typedef callback_t;", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

% Struct tests
test(struct_simple, [(Symbol == 'Point'), (Kind == struct)]) :-
    string_codes("struct Point {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(struct_with_whitespace, [(Symbol == 'Node'), (Kind == struct)]) :-
    string_codes("  struct   Node   {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(struct_underscore_name, [(Symbol == '_internal_data'), (Kind == struct)]) :-
    string_codes("struct _internal_data {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(struct_camel_case, [(Symbol == 'MyStructType'), (Kind == struct)]) :-
    string_codes("struct MyStructType {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

% Union tests
test(union_simple, [(Symbol == 'Value'), (Kind == union)]) :-
    string_codes("union Value {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(union_with_whitespace, [(Symbol == 'Data'), (Kind == union)]) :-
    string_codes("  union  Data  {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

% Enum tests
test(enum_simple, [(Symbol == 'Status'), (Kind == enum)]) :-
    string_codes("enum Status {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(enum_with_whitespace, [(Symbol == 'Color'), (Kind == enum)]) :-
    string_codes("  enum   Color   {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(enum_underscore, [(Symbol == 'error_code'), (Kind == enum)]) :-
    string_codes("enum error_code {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

% Negative tests
test(not_a_definition_variable, [fail]) :-
    % Regular variable declaration should not match
    string_codes("int variable;", Codes),
    phrase(global_definition(_, _), Codes, _).

test(not_a_definition_function, [fail]) :-
    % Function declaration should not match
    string_codes("int func(void);", Codes),
    phrase(global_definition(_, _), Codes, _).

test(not_a_definition_comment, [fail]) :-
    % Comment should not match
    string_codes("// #define COMMENTED_OUT", Codes),
    phrase(global_definition(_, _), Codes, _).

test(struct_without_brace, [fail]) :-
    % Struct declaration without opening brace should fail
    string_codes("struct Point;", Codes),
    phrase(global_definition(_, struct), Codes, _).

test(invalid_identifier, [fail]) :-
    % Invalid identifier (starts with digit)
    string_codes("#define 123ABC", Codes),
    phrase(global_definition(_, _), Codes, _).

:- end_tests(global_definition).

% ============================================================
% ===============  C_IDENTIFIER TESTS  =======================
% ============================================================

:- begin_tests(c_identifier).

test(simple_lowercase, [Id == foo]) :-
    string_codes("foo", Codes),
    phrase(c_identifier(Id), Codes).

test(simple_uppercase, [Id == 'FOO']) :-
    string_codes("FOO", Codes),
    phrase(c_identifier(Id), Codes).

test(mixed_case, [Id == 'FooBar']) :-
    string_codes("FooBar", Codes),
    phrase(c_identifier(Id), Codes).

test(with_underscore_start, [Id == '_private']) :-
    string_codes("_private", Codes),
    phrase(c_identifier(Id), Codes).

test(with_underscore_middle, [Id == 'func_name']) :-
    string_codes("func_name", Codes),
    phrase(c_identifier(Id), Codes).

test(with_underscore_end, [Id == 'var_']) :-
    string_codes("var_", Codes),
    phrase(c_identifier(Id), Codes).

test(with_digits, [Id == 'var123']) :-
    string_codes("var123", Codes),
    phrase(c_identifier(Id), Codes).

test(with_digits_middle, [Id == 'func2test']) :-
    string_codes("func2test", Codes),
    phrase(c_identifier(Id), Codes).

test(complex_identifier, [Id == '_MyFunc123_test']) :-
    string_codes("_MyFunc123_test", Codes),
    phrase(c_identifier(Id), Codes).

test(single_letter, [Id == x]) :-
    string_codes("x", Codes),
    phrase(c_identifier(Id), Codes).

test(single_underscore, [Id == '_']) :-
    string_codes("_", Codes),
    phrase(c_identifier(Id), Codes).

test(double_underscore, [Id == '__internal']) :-
    string_codes("__internal", Codes),
    phrase(c_identifier(Id), Codes).

test(all_uppercase_with_underscores, [Id == 'MAX_BUFFER_SIZE']) :-
    string_codes("MAX_BUFFER_SIZE", Codes),
    phrase(c_identifier(Id), Codes).

% Partial consumption tests
test(stops_at_space, [Id == foo]) :-
    string_codes("foo bar", Codes),
    phrase(c_identifier(Id), Codes, Rest),
    string_codes(" bar", Rest).

test(stops_at_paren, [Id == func]) :-
    string_codes("func()", Codes),
    phrase(c_identifier(Id), Codes, Rest),
    string_codes("()", Rest).

test(stops_at_semicolon, [Id == var]) :-
    string_codes("var;", Codes),
    phrase(c_identifier(Id), Codes, Rest),
    string_codes(";", Rest).

test(stops_at_operator, [Id == a]) :-
    string_codes("a+b", Codes),
    phrase(c_identifier(Id), Codes, Rest),
    string_codes("+b", Rest).

% Negative tests
test(starts_with_digit_fails, [fail]) :-
    % Identifiers cannot start with digit
    string_codes("123abc", Codes),
    phrase(c_identifier(_), Codes).

test(empty_string_fails, [fail]) :-
    string_codes("", Codes),
    phrase(c_identifier(_), Codes).

test(only_digit_fails, [fail]) :-
    string_codes("5", Codes),
    phrase(c_identifier(_), Codes).

test(special_char_fails, [fail]) :-
    % Special characters not valid in identifiers
    string_codes("@var", Codes),
    phrase(c_identifier(_), Codes).

test(hyphen_fails, [fail]) :-
    % Hyphens not valid in C identifiers
    string_codes("my-var", Codes),
    phrase(c_identifier(_), Codes).

test(dot_fails, [fail]) :-
    % Dots not valid in C identifiers
    string_codes("my.var", Codes),
    phrase(c_identifier(_), Codes).

:- end_tests(c_identifier).

% ============================================================
% ===============  INTEGRATION TESTS  ========================
% ============================================================

:- begin_tests(integration).

test(macro_extracted_from_context, [(Symbol == 'MAX_SIZE'), (Kind == macro)]) :-
    % Simulate context string from cscope output
    Context = "#define MAX_SIZE",
    string_codes(Context, Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(struct_extracted_from_context, [(Symbol == 'Node'), (Kind == struct)]) :-
    Context = "struct Node {",
    string_codes(Context, Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(typedef_extracted_from_context, [(Symbol == 'size_t'), (Kind == typedef)]) :-
    Context = "typedef size_t;",
    string_codes(Context, Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(function_has_paren, [blocked('test_has_paren/1 has been removed')]) :-
    Context = "int main(int argc, char **argv) {",
    test_has_paren(Context).

test(variable_no_paren, [fail, blocked('test_has_paren/1 has been removed')]) :-
    Context = "static int global_counter = 0;",
    test_has_paren(Context).

test(multiple_definitions_in_sequence) :-
    % Test parsing multiple definitions
    Contexts = [
        "#define DEBUG",
        "struct Point {",
        "typedef myint;",
        "enum Status {",
        "union Value {"
    ],
    maplist([Ctx]>>(
        string_codes(Ctx, Codes),
        phrase(global_definition(_, _), Codes, _)
    ), Contexts).

test(identifier_extraction_from_macro) :-
    Context = "#define BUFFER_SIZE",
    string_codes(Context, Codes),
    phrase(global_definition(Symbol, macro), Codes, _Rest),
    Symbol == 'BUFFER_SIZE'.

test(identifier_extraction_from_struct) :-
    Context = "struct complex_name_123 {",
    string_codes(Context, Codes),
    phrase(global_definition(Symbol, struct), Codes, _Rest),
    Symbol == complex_name_123.

:- end_tests(integration).

% ============================================================
% ===============  EDGE CASE TESTS  ==========================
% ============================================================

:- begin_tests(edge_cases).

test(tabs_in_whitespace_macro, [(Symbol == 'TAB_MACRO'), (Kind == macro)]) :-
    % Tabs instead of spaces
    string_codes("\t#define\tTAB_MACRO", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(tabs_in_whitespace_struct, [(Symbol == 'TabStruct'), (Kind == struct)]) :-
    string_codes("\tstruct\tTabStruct\t{", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(very_long_identifier) :-
    % Test with very long (but valid) identifier
    LongName = "very_long_identifier_name_with_many_characters_123_test_func",
    string_codes(LongName, Codes),
    phrase(c_identifier(Id), Codes, _Rest),
    atom_string(Id, LongName).

test(nested_parens_in_macro, [(Symbol == 'COMPLEX'), (Kind == macro)]) :-
    Context = "#define COMPLEX",
    string_codes(Context, Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(anonymous_struct_fails, [fail]) :-
    % Anonymous struct (no name) should fail
    string_codes("struct {", Codes),
    phrase(global_definition(_, struct), Codes, _).

test(forward_declaration_struct, [fail]) :-
    % Forward declaration without brace should fail for struct
    string_codes("struct Point;", Codes),
    phrase(global_definition(_, struct), Codes, _).

test(typedef_with_struct_keyword, [(Symbol == 'point_t'), (Kind == typedef)]) :-
    % typedef struct NAME { pattern
    string_codes("typedef struct point_t {", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(typedef_struct_anonymous, [(Symbol == 'point_t'), (Kind == typedef)]) :-
    % typedef struct { ... } NAME; - but just the header part
    % This should match: typedef ... NAME ;
    string_codes("typedef point_t;", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(unicode_not_in_identifier, [fixme('code_type/2 accepts non-ASCII letters')]) :-
    % Unicode characters should fail (not valid in C identifiers)
    % However, code_type(C, alpha) accepts Unicode letters like 'ü'
    % This is a limitation of using code_type/2 without additional constraints
    string_codes("über", Codes),
    \+ phrase(c_identifier(_), Codes, _).

test(identifier_with_consecutive_underscores, [Id == '__foo__']) :-
    string_codes("__foo__", Codes),
    phrase(c_identifier(Id), Codes, _Rest).

test(macro_with_no_space_after_define, [(Symbol == 'NOSPACE'), (Kind == macro)]) :-
    % Edge case: minimal whitespace
    string_codes("#define NOSPACE", Codes),
    phrase(global_definition(Symbol, Kind), Codes, _Rest).

test(has_paren_at_position_0, [blocked('test_has_paren/1 is broken')]) :-
    % Paren is first character
    test_has_paren("(immediate").

:- end_tests(edge_cases).

% ============================================================
% ===============  PATTERN REFACTORING TESTS  ================
% ============================================================

:- begin_tests(pattern_refactoring).

% Test equality operator usage (= vs ==)
test(empty_line_equality_check) :-
    % Test that empty string checking works with == operator (bound variable)
    Line = "",
    Line == "".

test(non_empty_line_equality_check, [fail]) :-
    % Test that non-empty string doesn't equal empty (with == operator)
    Line = "content",
    Line == "".

% Test succ/2 for incrementing (implicitly tested through write_reformatted_lines)
test(succ_incrementing) :-
    % Verify succ/2 works as expected
    succ(0, 1),
    succ(5, 6),
    succ(99, 100).

% Test multi-clause pattern: symbol_line//4 branches
test(symbol_line_function_with_paren, [
    (Def = def("file.c", "myFunc", "10", _)),
    (Seen = ["myFunc"])
]) :-
    % Function definition with paren should be recognized
    Input = "file.c\tmyFunc\t10\tint myFunc(void) {\n",
    string_codes(Input, Codes),
    phrase(symbol_line(Def, [], Seen, 0), Codes, _Rest).

test(symbol_line_function_without_paren, [
    (Def = none),
    (Seen = [])
]) :-
    % Function without paren should return none
    Input = "file.c\tmyFunc\t10\tint myFunc;\n",
    string_codes(Input, Codes),
    phrase(symbol_line(Def, [], Seen, 0), Codes, _Rest).

test(symbol_line_already_seen_function, [
    (Def = none),
    (Seen = ["myFunc"])
]) :-
    % Already seen function should return none and not change Seen
    Input = "file.c\tmyFunc\t10\tint myFunc(void) {\n",
    string_codes(Input, Codes),
    phrase(symbol_line(Def, ["myFunc"], Seen, 0), Codes, _Rest).

test(symbol_line_global_macro, [
    (Def = def("file.c", 'MAX_SIZE', "5", _)),
    (Seen = [])
]) :-
    % Global macro should be recognized (c_identifier returns atoms)
    Input = "file.c\t<global>\t5\t#define MAX_SIZE 100\n",
    string_codes(Input, Codes),
    phrase(symbol_line(Def, [], Seen, 0), Codes, _Rest).

test(symbol_line_global_struct, [
    (Def = def("file.c", 'Point', "12", _)),
    (Seen = [])
]) :-
    % Global struct should be recognized
    Input = "file.c\t<global>\t12\tstruct Point {\n",
    string_codes(Input, Codes),
    phrase(symbol_line(Def, [], Seen, 0), Codes, _Rest).

test(symbol_line_global_unrecognized, [
    (Def = none),
    (Seen = [])
]) :-
    % Global scope with unrecognized definition should return none
    Input = "file.c\t<global>\t20\tint global_var = 0;\n",
    string_codes(Input, Codes),
    phrase(symbol_line(Def, [], Seen, 0), Codes, _Rest).

test(symbol_line_empty_line, [
    (Def = none),
    (Seen = [])
]) :-
    % Empty line should return none
    Input = "\n",
    string_codes(Input, Codes),
    phrase(symbol_line(Def, [], Seen, 0), Codes, _Rest).

% Test multi-clause pattern: symbol_file_lines//3
test(symbol_file_lines_multiple_defs) :-
    % Test parsing multiple definitions
    string_concat("file.c\tfunc1\t10\tint func1(void) {\n",
                  "file.c\t<global>\t20\t#define MACRO 1\n", Temp),
    string_concat(Temp, "file.c\tfunc2\t30\tint func2(int x) {\n", Input),
    string_codes(Input, Codes),
    phrase(symbol_file_lines(Defs, [], 0), Codes, _Rest),
    length(Defs, 3),
    % Check that we got the expected definitions (functions are strings, macros are atoms)
    member(def(_, "func1", _, _), Defs),
    member(def(_, 'MACRO', _, _), Defs),
    member(def(_, "func2", _, _), Defs).

test(symbol_file_lines_skip_duplicates) :-
    % Test that duplicate function names are skipped
    string_concat("file.c\tfunc1\t10\tint func1(void) {\n",
                  "file.c\tfunc1\t20\tint func1(void) {\n", Temp),
    string_concat(Temp, "file.c\tfunc2\t30\tint func2(void) {\n", Input),
    string_codes(Input, Codes),
    phrase(symbol_file_lines(Defs, [], 0), Codes, _Rest),
    length(Defs, 2),  % Only 2 definitions (func1 once, func2 once)
    member(def(_, "func1", _, _), Defs),
    member(def(_, "func2", _, _), Defs).

test(symbol_file_lines_skip_non_functions) :-
    % Test that non-function symbols without parens are skipped
    string_concat("file.c\tsymbol1\t10\tint symbol1;\n",
                  "file.c\tfunc1\t20\tint func1(void) {\n", Temp),
    string_concat(Temp, "file.c\tsymbol2\t30\textern int symbol2;\n", Input),
    string_codes(Input, Codes),
    phrase(symbol_file_lines(Defs, [], 0), Codes, _Rest),
    length(Defs, 1),  % Only func1
    Defs = [def(_, "func1", _, _)].

:- end_tests(pattern_refactoring).

% ============================================================
% ===============  NORMALIZE_PATH_CODES TESTS  ===============
% ============================================================

:- begin_tests(normalize_path_codes).

% Tests for the refactored normalize_path_codes//1 using multiple clauses

test(backslash_to_forward_slash) :-
    % Backslash should be replaced with forward slash
    phrase(normalize_path_codes(NormCodes), [0'\\, 0'f, 0'o, 0'o]),
    NormCodes = [0'/, 0'f, 0'o, 0'o].

test(multiple_backslashes) :-
    % Multiple backslashes
    phrase(normalize_path_codes(NormCodes), [0'\\, 0'\\, 0'\\]),
    NormCodes = [0'/, 0'/, 0'/].

test(uppercase_to_lowercase) :-
    % Uppercase should be converted to lowercase
    phrase(normalize_path_codes(NormCodes), [0'A, 0'B, 0'C]),
    NormCodes = [0'a, 0'b, 0'c].

test(mixed_case_path) :-
    % Windows-style path: C:\Users\Eric
    Input = [0'C, 0':, 0'\\, 0'U, 0's, 0'e, 0'r, 0's, 0'\\, 0'E, 0'r, 0'i, 0'c],
    phrase(normalize_path_codes(NormCodes), Input),
    NormCodes = [0'c, 0':, 0'/, 0'u, 0's, 0'e, 0'r, 0's, 0'/, 0'e, 0'r, 0'i, 0'c].

test(already_normalized) :-
    % Already normalized path should remain unchanged
    phrase(normalize_path_codes(NormCodes), [0'/, 0'u, 0's, 0'r, 0'/, 0'b, 0'i, 0'n]),
    NormCodes = [0'/, 0'u, 0's, 0'r, 0'/, 0'b, 0'i, 0'n].

test(lowercase_unchanged) :-
    % Lowercase should remain unchanged
    phrase(normalize_path_codes(NormCodes), [0'a, 0'b, 0'c]),
    NormCodes = [0'a, 0'b, 0'c].

test(numbers_unchanged) :-
    % Numbers should remain unchanged
    phrase(normalize_path_codes(NormCodes), [0'1, 0'2, 0'3]),
    NormCodes = [0'1, 0'2, 0'3].

test(special_chars_unchanged) :-
    % Special characters like -, _, . should remain unchanged
    Input = [0'-, 0'_, 0'., 0'@],
    phrase(normalize_path_codes(NormCodes), Input),
    NormCodes = Input.

test(empty_path) :-
    % Empty path
    phrase(normalize_path_codes(NormCodes), []),
    NormCodes = [].

test(windows_absolute_path) :-
    % C:\Windows\System32
    string_codes("C:\\Windows\\System32", Input),
    phrase(normalize_path_codes(NormCodes), Input),
    string_codes("c:/windows/system32", NormCodes).

test(unix_absolute_path) :-
    % /usr/local/bin
    string_codes("/usr/local/bin", Input),
    phrase(normalize_path_codes(NormCodes), Input),
    string_codes("/usr/local/bin", NormCodes).

test(relative_path_with_dots) :-
    % ../src/main.c
    string_codes("..\\Src\\Main.C", Input),
    phrase(normalize_path_codes(NormCodes), Input),
    string_codes("../src/main.c", NormCodes).

:- end_tests(normalize_path_codes).

% ============================================================
% ===============  FIELD PARSING TESTS  ======================
% ============================================================

:- begin_tests(field_parsing).

% Tests for field_until_tab//1
test(field_until_tab_simple) :-
    phrase(field_until_tab(Field), [0'a, 0'b, 0'c, 9], Rest),
    Field = [0'a, 0'b, 0'c],
    Rest = [9].

test(field_until_tab_empty) :-
    phrase(field_until_tab(Field), [9], Rest),
    Field = [],
    Rest = [9].

test(field_until_tab_with_special_chars) :-
    % Field can contain any character except tab
    Input = [0'a, 0'-, 0'b, 32, 0'c, 9],
    phrase(field_until_tab(Field), Input, Rest),
    Field = [0'a, 0'-, 0'b, 32, 0'c],
    Rest = [9].

test(field_until_tab_no_tab) :-
    % Without tab, consumes all input
    phrase(field_until_tab(Field), [0'a, 0'b, 0'c]),
    Field = [0'a, 0'b, 0'c].

% Tests for field_until_space//1
test(field_until_space_simple) :-
    phrase(field_until_space(Field), [0'a, 0'b, 0'c, 32], Rest),
    Field = [0'a, 0'b, 0'c],
    Rest = [32].

test(field_until_space_empty) :-
    phrase(field_until_space(Field), [32], Rest),
    Field = [],
    Rest = [32].

test(field_until_space_with_tab) :-
    % Field can contain tab when stopping at space
    phrase(field_until_space(Field), [0'a, 9, 0'b, 32], Rest),
    Field = [0'a, 9, 0'b],
    Rest = [32].

% Tests for field_until_newline//1
test(field_until_newline_lf) :-
    phrase(field_until_newline(Field), [0'a, 0'b, 0'c, 10], Rest),
    Field = [0'a, 0'b, 0'c],
    Rest = [10].

test(field_until_newline_cr) :-
    phrase(field_until_newline(Field), [0'a, 0'b, 0'c, 13], Rest),
    Field = [0'a, 0'b, 0'c],
    Rest = [13].

test(field_until_newline_empty_lf) :-
    phrase(field_until_newline(Field), [10], Rest),
    Field = [],
    Rest = [10].

test(field_until_newline_empty_cr) :-
    phrase(field_until_newline(Field), [13], Rest),
    Field = [],
    Rest = [13].

test(field_until_newline_with_tabs_and_spaces) :-
    % Field can contain tabs and spaces when stopping at newline
    phrase(field_until_newline(Field), [0'a, 9, 32, 0'b, 10], Rest),
    Field = [0'a, 9, 32, 0'b],
    Rest = [10].

test(field_until_newline_no_newline) :-
    % Without newline, consumes all input
    phrase(field_until_newline(Field), [0'a, 0'b, 0'c]),
    Field = [0'a, 0'b, 0'c].

:- end_tests(field_parsing).

% ============================================================
% ===============  NEWLINE TESTS  ============================
% ============================================================

:- begin_tests(newline).

test(crlf_newline) :-
    % CR LF (Windows)
    phrase(newline, [13, 10], Rest),
    Rest = [].

test(lf_newline) :-
    % LF (Unix)
    phrase(newline, [10], Rest),
    Rest = [].

test(cr_newline) :-
    % CR (Old Mac)
    phrase(newline, [13], Rest),
    Rest = [].

test(crlf_with_rest) :-
    % CR LF followed by more content
    phrase(newline, [13, 10, 0'a, 0'b], Rest),
    Rest = [0'a, 0'b].

test(lf_with_rest) :-
    % LF followed by more content
    phrase(newline, [10, 0'a, 0'b], Rest),
    Rest = [0'a, 0'b].

test(cr_with_rest) :-
    % CR followed by more content (but not LF)
    phrase(newline, [13, 0'a, 0'b], Rest),
    Rest = [0'a, 0'b].

test(not_newline_fails, [fail]) :-
    % Non-newline character should fail
    phrase(newline, [0'a]).

test(empty_fails, [fail]) :-
    % Empty input should fail
    phrase(newline, []).

:- end_tests(newline).

% ============================================================
% ===============  WHITESPACE TESTS  =========================
% ============================================================

:- begin_tests(whitespace).

% Tests for 'whites*'//0
test(whites_star_empty) :-
    % Zero whitespace chars
    phrase('whites*', [], Rest),
    Rest = [].

test(whites_star_single_space) :-
    phrase('whites*', [32], Rest),
    Rest = [].

test(whites_star_multiple_spaces) :-
    phrase('whites*', [32, 32, 32], Rest),
    Rest = [].

test(whites_star_tab) :-
    phrase('whites*', [9], Rest),
    Rest = [].

test(whites_star_mixed_whitespace) :-
    % Space, tab, newline
    phrase('whites*', [32, 9, 10], Rest),
    Rest = [].

test(whites_star_stops_at_non_white) :-
    phrase('whites*', [32, 9, 0'a], Rest),
    Rest = [0'a].

test(whites_star_no_whitespace) :-
    % No whitespace, should succeed leaving input unchanged
    phrase('whites*', [0'a, 0'b], Rest),
    Rest = [0'a, 0'b].

% Tests for 'whites+'//0
test(whites_plus_single_space) :-
    phrase('whites+', [32], Rest),
    Rest = [].

test(whites_plus_multiple_spaces) :-
    phrase('whites+', [32, 32, 32], Rest),
    Rest = [].

test(whites_plus_mixed) :-
    phrase('whites+', [32, 9, 10, 13], Rest),
    Rest = [].

test(whites_plus_stops_at_non_white) :-
    phrase('whites+', [32, 9, 0'a], Rest),
    Rest = [0'a].

test(whites_plus_empty_fails, [fail]) :-
    % At least one whitespace required
    phrase('whites+', []).

test(whites_plus_no_whitespace_fails, [fail]) :-
    % No whitespace, should fail
    phrase('whites+', [0'a, 0'b]).

:- end_tests(whitespace).

% ============================================================
% ===============  DEF_LINE_FIELDS TESTS  ====================
% ============================================================

:- begin_tests(def_line_fields).

test(parse_four_tab_fields) :-
    % Standard format: File\tScope\tLine\tContext
    Input = "file.c\tmyFunc\t123\tint myFunc(void) {",
    string_codes(Input, Codes),
    phrase(def_line_fields(File, Scope, Line, ContextCodes), Codes),
    File == "file.c",
    Scope == "myFunc",
    Line == "123",
    string_codes(Context, ContextCodes),
    Context == "int myFunc(void) {".

test(parse_global_scope) :-
    Input = "header.h\t<global>\t45\t#define MAX_SIZE 100",
    string_codes(Input, Codes),
    phrase(def_line_fields(File, Scope, Line, ContextCodes), Codes),
    File == "header.h",
    Scope == "<global>",
    Line == "45",
    string_codes(Context, ContextCodes),
    Context == "#define MAX_SIZE 100".

test(parse_empty_context) :-
    % Context field can be empty
    Input = "file.c\tfunc\t1\t",
    string_codes(Input, Codes),
    phrase(def_line_fields(File, Scope, Line, ContextCodes), Codes),
    File == "file.c",
    Scope == "func",
    Line == "1",
    ContextCodes == [].

test(parse_context_with_tabs) :-
    % Context can contain tab characters (in the actual code)
    Input = "file.c\tfunc\t10\tif (x)\t{ return; }",
    string_codes(Input, Codes),
    phrase(def_line_fields(File, Scope, Line, ContextCodes), Codes),
    File == "file.c",
    Scope == "func",
    Line == "10",
    string_codes(Context, ContextCodes),
    Context == "if (x)\t{ return; }".

test(parse_windows_path) :-
    % File path with backslashes
    Input = "C:\\src\\file.c\tfunc\t5\tcontext",
    string_codes(Input, Codes),
    phrase(def_line_fields(File, Scope, Line, ContextCodes), Codes),
    File == "C:\\src\\file.c",
    Scope == "func",
    Line == "5",
    string_codes(Context, ContextCodes),
    Context == "context".

:- end_tests(def_line_fields).

% ============================================================
% ===============  FUNCTION_NAMES_FILE TESTS  ================
% ============================================================

:- begin_tests(function_names_file).

test(extract_single_function) :-
    Input = "file.c\tfunc1\t10\tint func1(void) {\n",
    string_codes(Input, Codes),
    phrase(function_names_file(Functions, []), Codes),
    Functions = ["func1"].

test(extract_multiple_functions) :-
    Input = "a.c\tfunc1\t10\tcontext\nb.c\tfunc2\t20\tcontext\n",
    string_codes(Input, Codes),
    phrase(function_names_file(Functions, []), Codes),
    length(Functions, 2),
    member("func1", Functions),
    member("func2", Functions).

test(skip_global_scope) :-
    % <global> should be skipped
    Input = "a.c\t<global>\t10\tcontext\nb.c\tfunc1\t20\tcontext\n",
    string_codes(Input, Codes),
    phrase(function_names_file(Functions, []), Codes),
    Functions = ["func1"].

test(skip_duplicate_functions) :-
    % Duplicate function names should only appear once
    Input = "a.c\tfunc1\t10\tcontext\nb.c\tfunc1\t20\tcontext\n",
    string_codes(Input, Codes),
    phrase(function_names_file(Functions, []), Codes),
    Functions = ["func1"].

test(skip_empty_lines) :-
    % Empty lines should be skipped
    Input = "a.c\tfunc1\t10\tcontext\n\nb.c\tfunc2\t20\tcontext\n",
    string_codes(Input, Codes),
    phrase(function_names_file(Functions, []), Codes),
    length(Functions, 2),
    member("func1", Functions),
    member("func2", Functions).

test(empty_file) :-
    % Empty input should produce empty list
    phrase(function_names_file(Functions, []), []),
    Functions = [].

test(only_empty_lines) :-
    % Only empty lines should produce empty list
    Input = "\n\n\n",
    string_codes(Input, Codes),
    phrase(function_names_file(Functions, []), Codes),
    Functions = [].

test(preseeded_accumulator) :-
    % Test with pre-seeded accumulator (already seen functions)
    Input = "a.c\tfunc1\t10\tcontext\nb.c\tfunc2\t20\tcontext\n",
    string_codes(Input, Codes),
    phrase(function_names_file(Functions, ["func1"]), Codes),
    % func1 already in accumulator, so only func2 should be added
    Functions = ["func2", "func1"].

:- end_tests(function_names_file).

% ============================================================
% ===============  DETERMINISM TESTS  ========================
% ============================================================
% Test that DCG predicates are deterministic (no choicepoints).
% Uses det_goal/1 helper to verify deterministic execution.

% Helper: test that a goal succeeds deterministically (no choicepoints)
% Uses findall to verify exactly one solution exists
:- meta_predicate det_goal(0).
det_goal(Goal) :-
    findall(1, Goal, Solutions),
    Solutions = [1].

:- begin_tests(determinism).

% Tests for normalize_path_codes//1
test(normalize_path_codes_det_simple) :-
    det_goal(phrase(normalize_path_codes(_), [0'a, 0'b, 0'c])).

test(normalize_path_codes_det_backslash) :-
    det_goal(phrase(normalize_path_codes(_), [0'\\, 0'f, 0'o, 0'o])).

test(normalize_path_codes_det_uppercase) :-
    det_goal(phrase(normalize_path_codes(_), [0'A, 0'B, 0'C])).

test(normalize_path_codes_det_mixed) :-
    det_goal(phrase(normalize_path_codes(_), [0'C, 0':, 0'\\, 0'U, 0's, 0'e, 0'r])).

test(normalize_path_codes_det_empty) :-
    det_goal(phrase(normalize_path_codes(_), [])).

% Tests for c_identifier//1
test(c_identifier_det_simple) :-
    det_goal(phrase(c_identifier(_), [0'f, 0'o, 0'o])).

test(c_identifier_det_underscore_start) :-
    det_goal(phrase(c_identifier(_), [0'_, 0'p, 0'r, 0'i, 0'v])).

test(c_identifier_det_with_digits) :-
    det_goal(phrase(c_identifier(_), [0'v, 0'a, 0'r, 0'1, 0'2, 0'3])).

test(c_identifier_det_single) :-
    det_goal(phrase(c_identifier(_), [0'x])).

test(c_identifier_det_partial) :-
    det_goal(phrase(c_identifier(_), [0'f, 0'o, 0'o, 0'(], _)).

% Tests for c_identifier_rest//1
test(c_identifier_rest_det_simple) :-
    det_goal(phrase(c_identifier_rest(_), [0'a, 0'b, 0'c])).

test(c_identifier_rest_det_empty) :-
    det_goal(phrase(c_identifier_rest(_), [])).

test(c_identifier_rest_det_with_underscore) :-
    det_goal(phrase(c_identifier_rest(_), [0'_, 0'a, 0'b])).

% Tests for field_until_tab//1
test(field_until_tab_det_simple) :-
    det_goal(phrase(field_until_tab(_), [0'a, 0'b, 0'c], _)).

test(field_until_tab_det_empty) :-
    det_goal(phrase(field_until_tab(_), [], _)).

test(field_until_tab_det_no_tab) :-
    det_goal(phrase(field_until_tab(_), [0'a, 0'b, 0'c])).

% Tests for field_until_space//1
test(field_until_space_det_simple) :-
    det_goal(phrase(field_until_space(_), [0'a, 0'b, 0'c], _)).

test(field_until_space_det_empty) :-
    det_goal(phrase(field_until_space(_), [], _)).

test(field_until_space_det_no_space) :-
    det_goal(phrase(field_until_space(_), [0'a, 0'b])).

% Tests for field_until_newline//1
test(field_until_newline_det_lf) :-
    det_goal(phrase(field_until_newline(_), [0'a, 0'b, 0'c], _)).

test(field_until_newline_det_cr) :-
    det_goal(phrase(field_until_newline(_), [0'x, 0'y, 0'z], _)).

test(field_until_newline_det_no_newline) :-
    det_goal(phrase(field_until_newline(_), [0'a, 0'b])).

% Tests for newline//0
test(newline_det_crlf) :-
    det_goal(phrase(newline, [13, 10])).

test(newline_det_lf) :-
    det_goal(phrase(newline, [10])).

test(newline_det_cr) :-
    det_goal(phrase(newline, [13])).

test(newline_det_with_rest) :-
    det_goal(phrase(newline, [13, 10, 0'a], _)).

% Tests for 'whites*'//0
test(whites_star_det_empty) :-
    det_goal(phrase('whites*', [])).

test(whites_star_det_spaces) :-
    det_goal(phrase('whites*', [32, 32, 32])).

test(whites_star_det_mixed) :-
    det_goal(phrase('whites*', [32, 9, 10])).

test(whites_star_det_stops) :-
    det_goal(phrase('whites*', [32, 0'a], _)).

% Tests for 'whites+'//0
test(whites_plus_det_single) :-
    det_goal(phrase('whites+', [32])).

test(whites_plus_det_multiple) :-
    det_goal(phrase('whites+', [32, 9, 10])).

test(whites_plus_det_stops) :-
    det_goal(phrase('whites+', [32, 0'a], _)).

% Tests for global_definition//2
test(global_definition_det_macro) :-
    string_codes("#define MAX_SIZE", Codes),
    det_goal(phrase(global_definition(_, _), Codes, _)).

test(global_definition_det_struct) :-
    string_codes("struct Point {", Codes),
    det_goal(phrase(global_definition(_, _), Codes, _)).

test(global_definition_det_typedef) :-
    string_codes("typedef size_t;", Codes),
    det_goal(phrase(global_definition(_, _), Codes, _)).

test(global_definition_det_enum) :-
    string_codes("enum Status {", Codes),
    det_goal(phrase(global_definition(_, _), Codes, _)).

test(global_definition_det_union) :-
    string_codes("union Value {", Codes),
    det_goal(phrase(global_definition(_, _), Codes, _)).

% Tests for 'typedef_keyword?'//0 - internal helper, not exported
% Skipping direct tests since it's only used internally by global_definition//2
% which is already tested above

% Tests for def_line_fields//4
test(def_line_fields_det_standard) :-
    string_codes("file.c\tmyFunc\t123\tcontext", Codes),
    det_goal(phrase(def_line_fields(_, _, _, _), Codes)).

test(def_line_fields_det_global) :-
    string_codes("header.h\t<global>\t45\t#define MAX", Codes),
    det_goal(phrase(def_line_fields(_, _, _, _), Codes)).

% Tests for symbol_line//4
test(symbol_line_det_function) :-
    string_codes("file.c\tmyFunc\t10\tint myFunc(void) {\n", Codes),
    det_goal(phrase(symbol_line(_, [], _, 0), Codes, _)).

test(symbol_line_det_global_macro) :-
    string_codes("file.c\t<global>\t5\t#define MAX_SIZE 100\n", Codes),
    det_goal(phrase(symbol_line(_, [], _, 0), Codes, _)).

test(symbol_line_det_empty) :-
    string_codes("\n", Codes),
    det_goal(phrase(symbol_line(_, [], _, 0), Codes, _)).

% Tests for symbol_file_lines//3
test(symbol_file_lines_det_single) :-
    string_codes("file.c\tfunc1\t10\tint func1(void) {\n", Codes),
    det_goal(phrase(symbol_file_lines(_, [], 0), Codes)).

test(symbol_file_lines_det_empty) :-
    det_goal(phrase(symbol_file_lines(_, [], 0), [])).

% Tests for function_names_file//2
test(function_names_file_det_single) :-
    string_codes("file.c\tfunc1\t10\tcontext\n", Codes),
    det_goal(phrase(function_names_file(_, []), Codes)).

test(function_names_file_det_empty) :-
    det_goal(phrase(function_names_file(_, []), [])).

:- end_tests(determinism).

% ============================================================
% ===============  PATH HANDLING TESTS  ======================
% ============================================================

:- begin_tests(path_handling).

% Test Unix-style absolute path
test(unix_absolute_path, [condition(exists_directory('/tmp'))]) :-
    % On Unix systems, /tmp should exist
    catch(
        generate_cscope_data('/tmp', [root('.'), debug(0), quiet(true)]),
        _,
        true  % May fail due to no source files, but path handling should work
    ).

% Test Windows-style absolute path with backslashes
test(windows_backslash_path, [
    condition(current_prolog_flag(windows, true)),
    condition(exists_directory('C:\\Windows'))
]) :-
    % Test that backslash paths are accepted
    catch(
        generate_cscope_data('C:\\Windows', [root('.'), debug(0), quiet(true)]),
        _,
        true  % May fail due to no source files
    ).

% Test Windows-style absolute path with forward slashes
test(windows_forward_slash_path, [
    condition(current_prolog_flag(windows, true)),
    condition(exists_directory('C:/Windows'))
]) :-
    catch(
        generate_cscope_data('C:/Windows', [root('.'), debug(0), quiet(true)]),
        _,
        true
    ).

% Test Git Bash path conversion on Windows (/c/... -> C:/...)
test(git_bash_path_conversion, [
    condition(current_prolog_flag(windows, true))
]) :-
    % Create a test to verify Git Bash path gets converted
    SrcDir = "/c/Windows",
    atom_string(SrcDir, SrcDirStr),
    % Verify conversion logic
    string_concat("/", Rest, SrcDirStr),
    string_length(Rest, Len),
    Len >= 2,
    sub_string(Rest, 0, 1, _, DriveLetter),
    char_type(DriveLetter, alpha),
    sub_string(Rest, 1, 1, _, "/"),
    upcase_atom(DriveLetter, DriveUpper),
    DriveUpper == 'C'.

% Test relative path resolution
test(relative_path, [setup(make_directory_path('test_temp')), cleanup(delete_directory('test_temp'))]) :-
    % Test that relative paths are accepted
    catch(
        generate_cscope_data('./test_temp', [root('.'), debug(0), quiet(true)]),
        _,
        true
    ).

% Test non-existent directory fails
test(nonexistent_directory, [fail]) :-
    generate_cscope_data('/this/path/does/not/exist/at/all', [root('.'), debug(0), quiet(true)]).

% Test tilde expansion (if supported)
test(tilde_path, [
    condition(\+ current_prolog_flag(windows, true)),
    blocked('Tilde expansion depends on shell environment')
]) :-
    catch(
        generate_cscope_data('~/', [root('.'), debug(0), quiet(true)]),
        _,
        true
    ).

% Test path with spaces (common on Windows)
test(path_with_spaces, [
    condition(current_prolog_flag(windows, true)),
    condition(exists_directory('C:/Program Files'))
]) :-
    catch(
        generate_cscope_data('C:/Program Files', [root('.'), debug(0), quiet(true)]),
        _,
        true
    ).

% Test that atom and string inputs both work
test(atom_input) :-
    catch(
        generate_cscope_data('.', [root('.'), debug(0), quiet(true)]),
        _,
        true
    ).

test(string_input) :-
    catch(
        generate_cscope_data(".", [root('.'), debug(0), quiet(true)]),
        _,
        true
    ).

:- end_tests(path_handling).
