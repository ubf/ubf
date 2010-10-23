%% @doc Low-level functions for encoding and decoding the UBF(A)
%% protocol.
%%
%%   UBF is a family of languages for transporting and describing
%%   complex data structures across a network.  It has three
%%   components.  In terms of a protocol stack, UBF(A) is a data
%%   transport format, roughly equivalent to well-formed XML.
%%
%% == Quick Summary ==
%%
%%   UBF(A) is the transport format, it was designed to be easy to
%%   parse and to be easy to write with a text editor. UBF(A) is based
%%   on a byte encoded virtual machine, 26 byte codes are
%%   reserved. Instead of allocating the byte codes from 0 we use the
%%   printable character codes to make the format easy to read.
%%
%% For more information, please see the following:
%% <ul>
%% <li> <a href="../priv/doc/doc/site/ubfa.html">Joe Armstrong's UBF(A) summary</a>  </li>
%% <li> <a href="../priv/doc/doc/site/ubfa_quick.html">Joe Armstrong's UBF(A) quick-reference summary</a>  </li>
%% </ul>
%%
%% == Hint: Decoding rules ==
%%
%% <ul>
%% <li> {'#S', String} -> String </li>
%% <li> Int            -> Int </li>
%% <li> [ ]            -> List </li>
%% <li> {...}          -> Tuple </li>
%% </ul>
%%

-module(ubf).
-behavior(contract_proto).

-export([proto_vsn/0, proto_driver/0, proto_packet_type/0]).
-export([encode/1, encode/2]).
-export([decode_init/0, decode/1, decode/2, decode/3]).
-export([deabstract/1]).

-import(lists, [foldl/3, reverse/1, map/2, seq/2, sort/1]).


%%---------------------------------------------------------------------
proto_vsn()         -> 'ubf1.0'.
proto_driver()      -> ubf_driver.
proto_packet_type() -> 0.


%%---------------------------------------------------------------------
decode_init() ->
    {more, fun(I) -> decode1(I, [[]], dict:new()) end}.

decode(Str) ->
    decode(Str, ?MODULE).

decode(Str, Mod) ->
    decode(Str, Mod, decode_init()).

decode(S, _Mod, {more, Fun}) ->
    Fun(S).

decode1([$'|T], Stack, Dict) ->
    get_stuff(T, $', [], Stack, Dict);
decode1([$~|T], [[Int|Stack]|S1], Dict) when is_integer(Int), Int >= 0 ->
    collect_binary(Int, T, [], [Stack|S1], Dict);
decode1([$~|_T], _Stack, _Dict) ->
    exit(tilde);
decode1([$%|T], Stack, Dict) ->
    get_stuff(T, $%, [], Stack, Dict);
decode1([$"|T], Stack, Dict) ->
    get_stuff(T, $", [], Stack, Dict);
decode1([$`|T], Stack, Dict) ->
    get_stuff(T, $`, [], Stack, Dict);
decode1([$-|T], Stack, Dict) ->
    collect_int(T, 0, '-', Stack, Dict);
decode1([H|T], Stack, Dict) when $0 =< H, H =< $9 ->
    collect_int(T, H-$0, '+', Stack, Dict);
decode1([${|T], Stack, Dict) ->
    decode1(T, [[]|Stack], Dict);
decode1([$}|T], [H|Stack], Dict) ->
    decode1(T, push(list_to_tuple(reverse(H)),Stack), Dict);
decode1([$&|T], [[H1,H2|T1] | Stack], Dict) ->
    decode1(T, [[[H1|H2]|T1]|Stack], Dict);
decode1([$#|T], Stack, Dict) ->
    decode1(T, push([], Stack), Dict);
decode1([$$|T], [[X]], _Dict) ->
    {ok, X, T};
decode1([$>], Stack, Dict) ->
    {more, fun(I) -> decode1([$>|I], Stack, Dict) end};
decode1([$>,Key|T], [[Val|R]|Stack], Dict) ->
    decode1(T, [R|Stack], dict:store(Key,Val,Dict));
decode1([H|T], Stack, Dict) ->
    case special(H) of
        true ->
            decode1(T, Stack, Dict);
        false ->
            decode1(T, push(dict:fetch(H, Dict), Stack), Dict)
    end;
decode1([], Stack, Dict) ->
    {more, fun(I) -> decode1(I, Stack, Dict) end};
decode1(_X, _Stack, _Dict) ->
    exit(decode1).

get_stuff([$\\], Stop, L, Stack, Dict) ->
    {more, fun(I) -> get_stuff([$\\|I], Stop, L, Stack, Dict) end};
get_stuff([$\\,H|T], Stop, L, Stack, Dict) ->
    get_stuff(T, Stop, [H|L], Stack, Dict);
get_stuff([$'|T], $', L, Stack, Dict)  ->
    decode1(T, push(list_to_atom(reverse(L)),Stack), Dict);
get_stuff([$"|T], $", L, Stack, Dict)  ->
    decode1(T, push({'#S',reverse(L)},Stack), Dict);
get_stuff([$`|T], $`, L, [[X|Top]|Stack], Dict)  ->
    decode1(T, push({'$TYPE', reverse(L), X}, [Top|Stack]), Dict);
get_stuff([$%|T], $%, _L, Stack, Dict)  ->
    decode1(T, Stack, Dict);
get_stuff([H|T], Stop, L, Stack, Dict) ->
    get_stuff(T, Stop, [H|L], Stack, Dict);
get_stuff([], Stop, L, Stack, Dict) ->
    {more, fun(I) -> get_stuff(I, Stop, L, Stack, Dict) end}.

collect_binary(0, T, L, Stack, Dict) ->
    expect_tilde(T, push(list_to_binary(reverse(L)),Stack), Dict);
collect_binary(N, [H|T], L, Stack, Dict) ->
    collect_binary(N-1, T, [H|L], Stack, Dict);
collect_binary(N, [], L, Stack, Dict) ->
    {more, fun(I) -> collect_binary(N, I, L, Stack, Dict) end}.

expect_tilde([$~|T], Stack, Dict) ->
    decode1(T, Stack, Dict);
expect_tilde([], Stack, Dict) ->
    {more, fun(I) -> expect_tilde(I, Stack, Dict) end};
expect_tilde([H|_], _, _) ->
    exit({expect_tilde, H}).

push(X, [Top|Rest]) ->
    [[X|Top]|Rest];
push(_X, _Y) ->
    exit(push).

special($ )  -> true;
special(${)  -> true;
special($})  -> true;
special($,)  -> true;
special($#)  -> true;
special($&)  -> true;
special($%)  -> true;
special($>)  -> true;
special($\n) -> true;
special($\r) -> true;
special($\t) -> true;
special($$)  -> true;
special($")  -> true;
special($')  -> true;
special($~)  -> true;
special(_)   -> false.

special_chars() ->
    " 0123456789{},~%#>\n\r\s\t\"'-&$".

collect_int([H|T], N, Sign, Stack, Dict) when $0 =< H, H =< $9 ->
    collect_int(T, N*10 + H - $0, Sign, Stack, Dict);
collect_int([], N, Sign, Stack, Dict) ->
    {more, fun(I) -> collect_int(I, N, Sign, Stack, Dict) end};
collect_int(T, N, '+', Stack, Dict) ->
    decode1(T, push(N, Stack), Dict);
collect_int(T, N, '-', Stack, Dict) ->
    decode1(T, push(-N, Stack), Dict).


%%---------------------------------------------------------------------
encode(X) ->
    encode(X, ?MODULE).

encode(X, _Mod) ->
    element(1, encode1(X, dict:new())).

encode1(X, Dict0) ->
    {Dict1, L1} = initial_dict(X, Dict0),
    case (catch do_encode(X, Dict1)) of
        {'EXIT', _What} ->
            exit(encode1);
        L ->
            {flatten([L1, L,$$]), Dict1}
    end.

initial_dict(X, Dict0) ->
    Free = seq(32,255) -- special_chars(),
    Most = analyse(X),
    load_dict(Most, Free, Dict0, []).

load_dict([{N,X}|T], [Key|T1], Dict0, L) when N > 0->
    load_dict(T, T1, dict:store(X, Key, Dict0),
              [encode_obj(X),">",Key|L]);
load_dict(_, _, Dict, L) ->
    {Dict, L}.

analyse(T) ->
    KV = dict:to_list(analyse(T, dict:new())),
    %% The Range is the Number of things times its size.  If the size
    %% is greater than 0
    KV1 = map(fun rank/1, KV),
    reverse(sort(KV1)).

rank({X, K}) when is_atom(X) ->
    case length(atom_to_list(X)) of
        N when N > 1, K > 1 ->
            {(N-1) * K, X};
        _ ->
            {0, X}
    end;
rank({X, K}) when is_integer(X) ->
    case length(integer_to_list(X)) of
        N when N > 1, K > 1 ->
            {(N-1) * K, X};
        _ ->
            {0, X}
    end;
rank({X, _}) ->
    {0, X}.

analyse({'#S', Str}, Dict) ->
    analyse(Str, Dict);
analyse(T, Dict) when is_tuple(T) ->
    foldl(fun analyse/2, Dict, tuple_to_list(T));
analyse(X, Dict) ->
    case dict:find(X, Dict) of
        {ok, Val} ->
            dict:store(X, Val+1, Dict);
        error ->
            dict:store(X, 1, Dict)
    end.

flatten(L) ->
    binary_to_list(list_to_binary(L)).

encode_obj(X) when is_atom(X) -> encode_atom(X);
encode_obj(X) when is_integer(X) -> integer_to_list(X);
encode_obj(X) when is_binary(X) -> encode_binary(X).

encode_string(S) -> [$",add_string(S, $"), $"].
encode_atom(X) -> [$',add_string(atom_to_list(X), $'), $'].
encode_binary(X) -> [integer_to_list(byte_size(X)), $~,X,$~].

do_encode(X, Dict) when is_atom(X); is_integer(X); is_binary(X) ->
    case dict:find(X, Dict) of
        {ok, Y} ->
            Y;
        error ->
            encode_obj(X)
    end;
do_encode({'#S', Str}, _Dict) ->
    %% This *is* a string
    encode_string(Str);
do_encode([H|T], Dict) ->
    S1  = do_encode(T, Dict),
    S2  = do_encode(H, Dict),
    [S1,S2,$&];
do_encode(T, Dict) when is_tuple(T) ->
    S1 = encode_tuple(1, T, Dict),
    [${,S1,$}];
do_encode([], _Dict) ->
    $#.

encode_tuple(N, T, _Dict) when N > tuple_size(T) ->
    "";
encode_tuple(N, T, Dict) ->
    S1 = do_encode(element(N, T), Dict),
    S2 = encode_tuple(N+1, T, Dict),
    [S1,possible_comma(N, T),S2].

possible_comma(N, T) when N < tuple_size(T) -> $,;
possible_comma(_, _) -> [].

%% The ascii printables are in the range 32..126 inclusive
add_string([$\\|T], Quote) -> [$\\,$\\|add_string(T, Quote)];
add_string([Quote|T], Quote) -> [$\\,Quote|add_string(T, Quote)];
add_string([H|T], Quote) when H >= 0,  H=< 255 -> [H|add_string(T, Quote)];
add_string([H|_], _Quote) -> exit({string_character,H});
add_string([], _) -> [].


%%---------------------------------------------------------------------
deabstract({'#S',S}) -> S;
deabstract(T) when is_tuple(T) ->
    list_to_tuple(map(fun deabstract/1, tuple_to_list(T)));
deabstract([H|T]) -> [deabstract(H)|deabstract(T)];
deabstract(T) -> T.
