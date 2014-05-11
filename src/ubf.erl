%%% The MIT License
%%%
%%% Copyright (C) 2011-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
%%% Copyright (C) 2002 by Joe Armstrong
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

%%% @doc Low-level functions for encoding and decoding the UBF(a)
%%% protocol.
%%%
%%% UBF is a family of languages for transporting and describing
%%% complex data structures across a network.  It has three
%%% components.  In terms of a protocol stack, UBF(a) is a data
%%% transport format, roughly equivalent to well-formed XML.
%%%
%%% UBF(a) is the transport format, it was designed to be easy to
%%% parse and to be easy to write with a text editor. UBF(a) is based
%%% on a byte encoded virtual machine, 26 byte codes are
%%% reserved. Instead of allocating the byte codes from 0 we use the
%%% printable character codes to make the format easy to read.
%%%

-module(ubf).
-behaviour(contract_proto).

-export([proto_vsn/0, proto_driver/0, proto_packet_type/0]).
-export([encode/1, encode/2]).
-export([decode_init/0, decode_init/1, decode_init/2, decode/1, decode/2, decode/3]).
-export([deabstract/1]).

-import(lists, [foldl/3, reverse/1, map/2, seq/2, sort/1]).

-record(state, {
          safe=false :: boolean(),
          dict :: term() % FYI - dict:dict() vs. dict() Erlang/OTP 17 incompatibility
         }).


%%---------------------------------------------------------------------
proto_vsn()         -> 'ubf1.0'.
proto_driver()      -> ubf_driver.
proto_packet_type() -> 0.


%%---------------------------------------------------------------------
decode_init() ->
    decode_init(false).

decode_init(Safe) ->
    decode_init(Safe, []).

decode_init(Safe, []) ->
    State = #state{safe=Safe, dict=dict:new()},
    {more, fun(I) -> decode1(I, [[]], State) end};
decode_init(Safe, String) when is_list(String) ->
    State = #state{safe=Safe, dict=dict:new()},
    decode1(String, [[]], State).

decode(String) ->
    decode(String, ?MODULE).

decode(String, Mod) ->
    decode(String, Mod, decode_init(false)).

decode(S, _Mod, {more, Fun}) ->
    Fun(S).

decode1([$'|T], Stack, State) ->
    get_stuff(T, $', [], Stack, State);
decode1([$~|T], [[Int|Stack]|S1], State) when is_integer(Int), Int >= 0 ->
    collect_binary(Int, T, [], [Stack|S1], State);
decode1([$~|_T], _Stack, _State) ->
    exit(tilde);
decode1([$%|T], Stack, State) ->
    get_stuff(T, $%, [], Stack, State);
decode1([$"|T], Stack, State) ->
    get_stuff(T, $", [], Stack, State);
decode1([$`|T], Stack, State) ->
    get_stuff(T, $`, [], Stack, State);
decode1([$-|T], Stack, State) ->
    collect_int(T, 0, '-', Stack, State);
decode1([H|T], Stack, State) when $0 =< H, H =< $9 ->
    collect_int(T, H-$0, '+', Stack, State);
decode1([${|T], Stack, State) ->
    decode1(T, [[]|Stack], State);
decode1([$}|T], [H|Stack], State) ->
    decode1(T, push(list_to_tuple(reverse(H)),Stack), State);
decode1([$&|T], [[H1,H2|T1] | Stack], State) ->
    decode1(T, [[[H1|H2]|T1]|Stack], State);
decode1([$#|T], Stack, State) ->
    decode1(T, push([], Stack), State);
decode1([$$|T], [[X]], _State) ->
    {done, X, T, undefined};
decode1([$>], Stack, State) ->
    {more, fun(I) -> decode1([$>|I], Stack, State) end};
decode1([$>,Key|T], [[Val|R]|Stack], #state{dict=Dict}=State) ->
    decode1(T, [R|Stack], State#state{dict=dict:store(Key,Val,Dict)});
decode1([H|T], Stack, #state{dict=Dict}=State) ->
    case special(H) of
        true ->
            decode1(T, Stack, State);
        false ->
            decode1(T, push(dict:fetch(H, Dict), Stack), State)
    end;
decode1([], Stack, State) ->
    {more, fun(I) -> decode1(I, Stack, State) end};
decode1(_X, _Stack, _State) ->
    exit(decode1).

get_stuff([$\\], Stop, L, Stack, State) ->
    {more, fun(I) -> get_stuff([$\\|I], Stop, L, Stack, State) end};
get_stuff([$\\,H|T], Stop, L, Stack, State) ->
    get_stuff(T, Stop, [H|L], Stack, State);
get_stuff([$'|T], $', L, Stack, #state{safe=Safe}=State) ->
    RevL = reverse(L),
    Atom = if Safe -> list_to_existing_atom(RevL); true -> list_to_atom(RevL) end,
    decode1(T, push(Atom, Stack), State);
get_stuff([$"|T], $", L, Stack, State)  ->
    decode1(T, push({'#S',reverse(L)},Stack), State);
get_stuff([$`|T], $`, L, [[X|Top]|Stack], State)  ->
    decode1(T, push({'$TYPE', reverse(L), X}, [Top|Stack]), State);
get_stuff([$%|T], $%, _L, Stack, State)  ->
    decode1(T, Stack, State);
get_stuff([H|T], Stop, L, Stack, State) ->
    get_stuff(T, Stop, [H|L], Stack, State);
get_stuff([], Stop, L, Stack, State) ->
    {more, fun(I) -> get_stuff(I, Stop, L, Stack, State) end}.

collect_binary(0, T, L, Stack, State) ->
    expect_tilde(T, push(list_to_binary(reverse(L)),Stack), State);
collect_binary(N, [H|T], L, Stack, State) ->
    collect_binary(N-1, T, [H|L], Stack, State);
collect_binary(N, [], L, Stack, State) ->
    {more, fun(I) -> collect_binary(N, I, L, Stack, State) end}.

expect_tilde([$~|T], Stack, State) ->
    decode1(T, Stack, State);
expect_tilde([], Stack, State) ->
    {more, fun(I) -> expect_tilde(I, Stack, State) end};
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

collect_int([H|T], N, Sign, Stack, State) when $0 =< H, H =< $9 ->
    collect_int(T, N*10 + H - $0, Sign, Stack, State);
collect_int([], N, Sign, Stack, State) ->
    {more, fun(I) -> collect_int(I, N, Sign, Stack, State) end};
collect_int(T, N, '+', Stack, State) ->
    decode1(T, push(N, Stack), State);
collect_int(T, N, '-', Stack, State) ->
    decode1(T, push(-N, Stack), State).


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

analyse({'#S', String}, Dict) ->
    analyse(String, Dict);
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
do_encode({'#S', String}, _Dict) ->
    %% This *is* a string
    encode_string(String);
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
