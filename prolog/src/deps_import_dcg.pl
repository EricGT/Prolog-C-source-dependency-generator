:- module(deps_import, [
    import_make_deps/3,          % +MakeFile, :AssertIncludes, +Options
    import_ctags_jsonl/2         % unchanged helper (kept for convenience)
]).

:- use_module(library(readutil)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(option)).

%% import_make_deps(+MakeDepsFile, :AssertIncludes, +Options) is det.
% Options:
%   keep(system)                  % keep all headers (default)
%   keep(project(Prefix))         % keep only headers whose path contains Prefix
%   debug(true/false)             % print debug info (default false)
import_make_deps(Path, Assert, Opts) :-
    option(debug(Debug), Opts, false),
    setup_call_cleanup(open(Path, read, S, [encoding(utf8)]),
      ( read_stream_to_codes(S, Codes0),
        % 1) strip ANSI/OSC/CSI
        phrase(strip_ansi(Codes1), Codes0),
        % 2) join backslash-newline continuations
        phrase(join_continuations(Codes2), Codes1),
        % 3) split into lines (lists of codes), drop blanks
        lines_nonempty(Codes2, Lines),
        % 4) parse lines and assert includes
        option(keep(Filter), Opts, system),
        State0 = state(0,0,0,0,[]),
        foldl(process_line(Assert, Filter, Debug), Lines, State0, state(NP,NL,NK,NE,Samples)),
        ( Debug == true ->
            format("import_make_deps: processed=~d, parsed=~d, kept=~d, edges=~d~n", [NP,NL,NK,NE]),
            ( Samples == [] -> true
            ; format("  sample edges:~n", []),
              forall(member(Src-H, Samples), format("    ~s -> ~s~n", [Src,H]))
            )
        ; true )
      ),
      close(S)).

% ---------- Step 1: strip ANSI/OSC/CSI escape sequences via DCG ----------

% ESC = 27, BEL = 7. We handle:
%  - OSC: ESC ] ... BEL   or   ESC ] ... ESC \
%  - CSI: ESC [ ... final byte 0x40–0x7E
%  - Single-character CSI-like sequences ESC <char> we just pass through unless '[' or ']'

strip_ansi(Out) --> ansi_chunk(Out).
ansi_chunk([C|Out]) --> [C], {C =\= 27}, !, strip_ansi(Out).
ansi_chunk(Out)     --> [27,93], !, osc_seq(Out).           % ESC ]
ansi_chunk(Out)     --> [27,91], !, csi_seq(Out).           % ESC [
ansi_chunk(Out)     --> [27,_], !, strip_ansi(Out).         % ESC <other>: drop it
ansi_chunk([])      --> [].

% OSC: consume until BEL (7) OR ESC \
osc_seq(Out) --> until_bel_or_st, !, strip_ansi(Out).
until_bel_or_st --> [7], !.                               % BEL terminator
until_bel_or_st --> [27,0'\\], !.                         % ESC \
until_bel_or_st --> [_], until_bel_or_st.

% CSI: ESC [ parameters ... final in 0x40..0x7E
csi_seq(Out) --> csi_body, !, strip_ansi(Out).
csi_body --> [C], {C>=0'@, C=<0'~}, !.                     % final byte
csi_body --> [_], csi_body.

% ---------- Step 2: join backslash + optional spaces + newline ----------

join_continuations(Out) --> "\\", spcs, nl, !, " ", join_continuations(Out).
join_continuations([C|Out]) --> [C], !, join_continuations(Out).
join_continuations([]) --> [].

spc  --> [C], { member(C, [9,32]) }.   % tab or space
spcs --> spc, !, spcs ; [].

nl   --> [13,10], ! ; [10], ! ; [13], !.

% ---------- Step 3: split into nonempty lines ----------

lines_nonempty(Codes, Lines) :-
    split_lines(Codes, Ls),
    exclude(blank_line, Ls, Lines).

split_lines([], [[]]).
split_lines([C|Cs], [[]|Rest]) :- (C=10;C=13), !, eat_crlf(Cs, Cs1), split_lines(Cs1, Rest).
split_lines([C|Cs], [[C|L]|Rest]) :- split_lines(Cs, [L|Rest]).

eat_crlf([13,10|T], T) :- !.
eat_crlf([10,13|T], T) :- !.
eat_crlf([10|T], T) :- !.
eat_crlf([13|T], T) :- !.
eat_crlf(L, L).

blank_line([]).
blank_line(L) :- \+ ( member(C,L), \+ code_type(C, space) ).

% ---------- Step 4: parse "target: RHS ..." lines safely ----------

process_line(Assert, Filter, Debug, LineCodes,
             state(P0,L0,K0,E0,S0),
             state(P1,L1,K1,E1,S1)) :-
    P1 is P0 + 1,
    ( parse_dep_line(LineCodes, _Target, RHS), RHS \= [] ->
        L1 is L0 + 1,
        RHS = [SrcCodes|HeaderCodes],
        % Convert codes → strings once
        string_codes(SrcStr, SrcCodes),
        maplist(string_codes, HeaderStrs, HeaderCodes),
        ( is_c_source_str(SrcStr) ->
            include(keep_header_str(Filter), HeaderStrs, KeptStrs),
            length(KeptStrs, AddK), K1 is K0 + AddK,
            % assert edges
            forall(member(HStr, KeptStrs), call(Assert, SrcStr, HStr)),
            length(KeptStrs, AddE), E1 is E0 + AddE,
            ( Debug == true -> take_samples_str(S0, SrcStr, KeptStrs, S1)
            ; S1 = S0 )
        ; K1 = K0, E1 = E0, S1 = S0 )
    ; L1 = L0, K1 = K0, E1 = E0, S1 = S0 ).

take_samples(S0, _Src, [], S0).
take_samples(S0, Src, [H|Hs], S1) :-
    ( length(S0,N), N < 5
    -> append(S0, [Src-H], Sx)
    ;  Sx = S0 ),
    take_samples(Sx, Src, Hs, S1).

% Parse: <anything>: <files...>
parse_dep_line(Line, Target, Files) :-
    % find first ':' position
    ( append(Target, [0':|Rest], Line) ->
        skip_ws(Rest, RHS),
        ( RHS == [] -> Files = []
        ; split_ws(RHS, Files) )
    ; fail ).

skip_ws([C|Cs], Out) :- (C=32;C=9), !, skip_ws(Cs, Out).
skip_ws(L, L).

split_ws([], []).
split_ws(L, [Tok|Rest]) :-
    take_non_ws(L, Tok, R1),
    skip_ws(R1, R2),
    split_ws(R2, Rest).

take_non_ws([C|Cs], [C|T], R) :- C=\=32, C=\=9, !, take_non_ws(Cs, T, R).
take_non_ws(L, [], L).
take_non_ws([], [], []).

is_c_source(Cs) :-
    downcase_codes(Cs, LCs),
    ( ends_with_codes(LCs, ".c")
    ; ends_with_codes(LCs, ".cc")
    ; ends_with_codes(LCs, ".cpp")
    ; ends_with_codes(LCs, ".cxx")
    ; ends_with_codes(LCs, ".i")
    ; ends_with_codes(LCs, ".ii")
    ).

% ---- replace these helpers in deps_import_dcg.pl ----

is_c_source_str(S) :-
    string_lower(S, L),
    ( ends_with_str(L, ".c")
    ; ends_with_str(L, ".cc")
    ; ends_with_str(L, ".cpp")
    ; ends_with_str(L, ".cxx")
    ; ends_with_str(L, ".i")
    ; ends_with_str(L, ".ii")
    ).

ends_with_str(Str, Suf) :-
    string_length(Str, L1),
    string_length(Suf, L2),
    B is L1 - L2,
    B >= 0,
    sub_string(Str, B, L2, 0, Suf).

keep_header_str(system, _).
keep_header_str(project(Prefix), H) :-
    sub_string(H, _, _, _, Prefix).

take_samples_str(S0, _Src, [], S0).
take_samples_str(S0, Src, [H|Hs], S1) :-
    ( length(S0,N), N < 5 -> append(S0, [Src-H], Sx) ; Sx = S0 ),
    take_samples_str(Sx, Src, Hs, S1).

ends_with_codes(List, SuffixStr) :-
    string_codes(SuffixStr, Suf),
    append(_, Suf, List).

downcase_codes(In, Out) :-
    maplist(downcase_code, In, Out).
downcase_code(C, D) :- char_code(LC, C), downcase_char(LC, LD), char_code(LD, D).
downcase_char(C, D) :- char_type(C, to_lower(D)), !.
downcase_char(C, C).

keep_header(system, _H).
keep_header(project(Prefix), HCodes) :-
    string_codes(Prefix, Pfx),
    sublist(Pfx, HCodes).

% naive "substring" check on codes
sublist(Sub, List) :- append(_, Tail, List), append(Sub, _, Tail), !.

% ---------- ctags JSONL helper (unchanged) ----------
import_ctags_jsonl(JsonL, AssertProvides) :-
    setup_call_cleanup(open(JsonL, read, S, [encoding(utf8)]),
      import_ctags_stream(S, AssertProvides),
      close(S)).

import_ctags_stream(S, AssertProvides) :-
    read_line_to_string(S, Line),
    ( Line == end_of_file -> true
    ; ( catch(atom_json_dict(Line, D, []), _, fail),
        _ = D.get("_type"), D."_type" = "tag",
        F = D.path, N = D.name, K = D.kind, L = D.line,
        call(AssertProvides, F, N, K, L)
      ; true ),
      import_ctags_stream(S, AssertProvides)
    ).
