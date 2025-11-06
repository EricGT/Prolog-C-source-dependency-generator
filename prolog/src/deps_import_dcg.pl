:- module(deps_import, [
    % ---- Importers (callbacks OR convenience wrappers) ----
    import_make_deps/3,              % +MakeFile, :Assert(Src,H), +Options
    import_make_deps_into/3,         % +MakeFile, +IntoModule, +Options
    import_ctags_jsonl/2,            % +Jsonl, :Assert(File,Name,Kind,Line)
    import_ctags_jsonl_into/3,       % +Jsonl, +IntoModule, +Options (unused now)

    % ---- Normalization / materialization ----
    normalize_includes_into/2,       % +FromModule, +ToModule (writes ToModule:inc(SrcA,HdrA))

    % ---- Analyses (module-parameterized) ----
    depends_on_in/3,                 % +Module, ?From, ?To  (uses Module:inc/2)
    build_graph_in/2,                % +Module, -Graph     (ugraphs)
    % sccs_in/2,                       % +Module, -SCCs
    has_cycle_in/1,                  % +Module
    top_headers_in/3,                % +Module, +K, -Pairs(HeaderAtom,Count)

    % ---- Export helpers ----
    save_includes_from/2,            % +FromModule, +OutFile  (dumps includes/2)
    save_inc_from/2,                 % +FromModule, +OutFile  (dumps inc/2)
    to_dot_from/3                    % +FromModule, +DotFile, +Options
]).

/** <module> Dependency + symbol import, normalization, and graph utilities.

Options for import_make_deps[...]:
  - keep(system)                  keep all headers (default)
  - keep(project(PrefixString))   only keep headers whose *string* path contains PrefixString
  - debug(true/false)             print summary + samples (default false)
  - into(Module)                  (import_make_deps/3 only) target module for the assertion callback

Notes:
  - The deps importer reads a Makefile-style deps file (e.g., from `clang -M -MG`).
  - It strips ANSI/OSC/CSI escapes and joins backslash-newline continuations.
  - Assertions land in the module you choose (e.g., `user`).

Typical flow at the REPL:
  ?- [deps_import_dcg].
  ?- import_make_deps_into('.../deps.make', user, [keep(project("sqlite-src-3510000/src")), debug(true)]).
  ?- normalize_includes_into(user, user).  % materializes user:inc/2
  ?- depends_on_in(user, From, To).
*/

:- use_module(library(readutil)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(yall)).          % for {…}/[Args]>> lambdas
:- use_module(library(ugraphs)).

% :- dynamic includes/2. % TODO: Is this needed?

% ============================================================
% ===============  MAKE DEPS IMPORT (DCG)  ===================
% ============================================================

%% import_make_deps(+MakeDepsFile, :AssertIncludes, +Options) is det.
%  AssertIncludes must be callable as AssertIncludes(SrcStr, HdrStr).
%  Use import_make_deps_into/3 to avoid module-scoping hassles.
import_make_deps(Path, Assert, Opts) :-
    option(debug(Debug), Opts, false),
    setup_call_cleanup(
      open(Path, read, S, [encoding(utf8)]),
      ( read_stream_to_codes(S, Codes0),
        % (1) strip ANSI/OSC/CSI
        phrase(strip_ansi(Codes1), Codes0),
        % (2) join backslash-newline continuations
        phrase(join_continuations(Codes2), Codes1),
        % (3) split into non-empty lines (lists of codes)
        lines_nonempty(Codes2, Lines),
        % (4) parse lines and assert includes
        option(keep(Filter), Opts, system),
        State0 = state(0,0,0,0,[]),  % processed, parsed, kept, edges, samples
        foldl({Opts,Assert,Filter,Debug}/[Line,S0,S1]>>process_line(Opts,Assert,Filter,Debug,Line,S0,S1),
              Lines, State0, state(NP,NL,NK,NE,Samples)),
        ( Debug == true ->
            format("import_make_deps: processed=~d, parsed=~d, kept=~d, edges=~d~n", [NP,NL,NK,NE]),
            ( Samples == [] -> true
            ; format("  sample edges:~n", []),
              forall(member(Src-H, Samples), format("    ~s -> ~s~n", [Src,H]))
            )
        ; true )
      ),
      close(S)).

%% import_make_deps_into(+MakeDepsFile, +IntoModule, +Options) is det.
%  Convenience wrapper that asserts IntoModule:includes(S,H)
import_make_deps_into(Path, IntoMod, Opts) :-
    import_make_deps(Path,
      {IntoMod}/[S,H]>>assertz(IntoMod:includes(S,H)),
      Opts).

% ----- internals for deps importer -----

% Step 1: strip ANSI/OSC/CSI escape sequences
strip_ansi(Out) --> ansi_chunk(Out).
ansi_chunk([C|Out]) --> [C], {C =\= 27}, !, strip_ansi(Out).
ansi_chunk(Out)     --> [27,93], !, osc_seq(Out).          % ESC ]
ansi_chunk(Out)     --> [27,91], !, csi_seq(Out).          % ESC [
ansi_chunk(Out)     --> [27,_], !, strip_ansi(Out).        % ESC <other>: drop it
ansi_chunk([])      --> [].

% OSC: ESC ] ... BEL (7) or ESC \
osc_seq(Out) --> until_bel_or_st, !, strip_ansi(Out).
until_bel_or_st --> [7], !.
until_bel_or_st --> [27,0'\\], !.
until_bel_or_st --> [_], until_bel_or_st.

% CSI: ESC [ ... final byte 0x40–0x7E
csi_seq(Out) --> csi_body, !, strip_ansi(Out).
csi_body --> [C], {C>=0'@, C=<0'~}, !.
csi_body --> [_], csi_body.

% Step 2: join backslash + optional spaces + newline -> single space
join_continuations(Out) --> "\\", spcs, nl, !, " ", join_continuations(Out).
join_continuations([C|Out]) --> [C], !, join_continuations(Out).
join_continuations([]) --> [].

spc  --> [C], { member(C, [9,32]) }.   % tab or space
spcs --> spc, !, spcs ; [].

nl   --> [13,10], ! ; [10], ! ; [13], !.

% Step 3: split into non-empty lines
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

% Step 4: parse and assert
process_line(_Opts,_Assert,_Filter,_Debug, LineCodes,
             state(P0,L0,K0,E0,S0),
             state(P1,L1,K1,E1,S1)) :-
    P1 is P0 + 1,
    ( parse_dep_line(LineCodes, _Target, RHS), RHS \= [] ->
        L1 is L0 + 1,
        RHS = [SrcCodes|HeaderCodes],
        string_codes(SrcStr, SrcCodes),
        maplist(string_codes, HeaderStrs, HeaderCodes),
        ( is_c_source_str(SrcStr) ->
            option(keep(Filter), _Opts, system),
            include(keep_header_str(Filter), HeaderStrs, KeptStrs),
            length(KeptStrs, AddK), K1 is K0 + AddK,
            option(into(Mod), _Opts, user),
            forall(member(HStr, KeptStrs), assertz(Mod:includes(SrcStr, HStr))),
            length(KeptStrs, AddE), E1 is E0 + AddE,
            option(debug(Debug), _Opts, false),
            ( Debug == true -> take_samples_str(S0, SrcStr, KeptStrs, S1)
            ; S1 = S0 )
        ; K1 = K0, E1 = E0, S1 = S0 )
    ; L1 = L0, K1 = K0, E1 = E0, S1 = S0 ).

% parse "<target>: <rhs files...>"
parse_dep_line(Line, Target, Files) :-
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
keep_header_str(project(Prefix), H) :- sub_string(H, _, _, _, Prefix).

take_samples_str(S0, _Src, [], S0).
take_samples_str(S0, Src, [H|Hs], S1) :-
    ( length(S0,N), N < 5 -> append(S0, [Src-H], Sx) ; Sx = S0 ),
    take_samples_str(Sx, Src, Hs, S1).

% ============================================================
% ===================  CTAGS JSONL IMPORT  ===================
% ============================================================

%% import_ctags_jsonl(+JsonL, :AssertProvides) is det.
%  AssertProvides(File, Name, Kind, Line)
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

%% import_ctags_jsonl_into(+JsonL, +IntoModule, +Options) is det.
%  Convenience wrapper to assert IntoModule:provides(File,Name,Kind,Line)
import_ctags_jsonl_into(JsonL, IntoMod, _Opts) :-
    import_ctags_jsonl(JsonL, {IntoMod}/[F,N,K,L]>>assertz(IntoMod:provides(F,N,K,L))).

% ============================================================
% ===============  NORMALIZATION / MATERIALIZE  ==============
% ============================================================

%% normalize_includes_into(+FromModule, +ToModule) is det.
%  Reads FromModule:includes/2 (strings), writes ToModule:inc/2 (lowercased atoms), deduped.
normalize_includes_into(From, To) :-
    retractall(To:inc(_,_)),
    setof(Src-Hdr, From:includes(Src,Hdr), Pairs), !,
    forall(member(Src-Hdr, Pairs),
      ( string_lower(Src, SL), string_lower(Hdr, HL),
        atom_string(SA, SL), atom_string(HA, HL),
        ( To:inc(SA,HA) -> true ; assertz(To:inc(SA,HA)) ))).
normalize_includes_into(_, _).   % if no includes/2, succeed quietly

% ============================================================
% ========================  ANALYSES  ========================
% ============================================================

%% depends_on_in(+Module, ?From, ?To) using Module:inc/2
depends_on_in(M, A, B) :- M:inc(A,B).
depends_on_in(M, A, B) :- M:inc(A,Mid), depends_on_in(M, Mid, B).

%% build_graph_in(+Module, -Graph)
build_graph_in(M, G) :-
    findall(S, M:inc(S,_), Ss0),
    sort(Ss0, Ss),
    findall(H, M:inc(_,H), Hs0),
    sort(Hs0, Hs),
    ord_union(Ss, Hs, Vs),
    findall(S-H, M:inc(S,H), Es),
    vertices_edges_to_ugraph(Vs, Es, G).

%% sccs_in(+Module, -SCCs)
% sccs_in(M, SCCs) :-
%     build_graph_in(M, G),
%     strongly_connected_components(G, SCCs).

%% has_cycle_in(+Module)
has_cycle_in(M) :-
    build_graph_in(M, G),
    \+ acyclic_graph(G).

acyclic_graph(G) :-
    catch(top_sort(G, _),
          E,
          (   E = error(domain_error(acyclic_graph,_), _)
          ->  fail
          ;   throw(E)
          )).

%% top_headers_in(+Module, +K, -TopPairs)
%  Returns TopPairs as list of Header-Count (Header is atom, lowercased)
top_headers_in(M, K, Top) :-
    findall(H, M:inc(_,H), Hs),
    msort(Hs, Sorted),
    clumped(Sorted, Clumped),             % [H-N, ...]
    sort(2, @>=, Clumped, Ranked),
    length(Top, K),
    append(Top, _, Ranked).

% ============================================================
% =======================  SAVE / DOT  =======================
% ============================================================

%% save_includes_from(+FromModule, +OutFile)
save_includes_from(M, File) :-
  setup_call_cleanup(
    open(File, write, S, [encoding(utf8)]),
    forall(M:includes(Src,H),
           format(S, "includes(~q, ~q).~n", [Src,H])),
    close(S)).

%% save_inc_from(+FromModule, +OutFile)
save_inc_from(M, File) :-
  setup_call_cleanup(
    open(File, write, S, [encoding(utf8)]),
    forall(M:inc(Src,H),
           format(S, "inc(~q, ~q).~n", [Src,H])),
    close(S)).

%% to_dot_from(+FromModule, +DotFile, +Options)
% Options: rankdir('LR'|'TB'), node_fontsize(Int, default 9)
to_dot_from(M, DotFile, Options) :-
  option(rankdir(RD), Options, 'LR'),
  option(node_fontsize(FS), Options, 9),
  setup_call_cleanup(
    open(DotFile, write, S, [encoding(utf8)]),
    ( format(S, "digraph G { rankdir=~w; node [shape=box,fontsize=~d];~n", [RD,FS]),
      forall(M:inc(Src,H),
             format(S, "  \"~w\" -> \"~w\";~n", [Src,H])),
      format(S, "}~n", [])
    ),
    close(S)).
