%%% The MIT License
%%%
%%% Copyright (C) 2011 by Joseph Wayne Norton <norton@alum.mit.edu>
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

-module(ubf_test).

-compile(export_all).

-import(ubf, [encode/1, decode/1]).
-import(lists, [foldl/3, reverse/1, map/2, seq/2, sort/1]).

tests() ->
    test(),
    test1(),
    test2(),
    test9(),
    test10(),
    test11(),
    bug1(),
    bug2().

test() ->
    decode("'person' >p # {p 1479 -22} &"
           "  {p \"bingo\" 23} & $").

test1() ->
    decode("#5&4&3&2&1&$").

test2() ->
    L = encode(abc),
    L = "'abc'$",
    L.

test9() ->
    test_ubf({abc,"kdjhkshfkhfkhsfkhaf", [a,c,d]}).

test10() ->
    T = epp:parse_file("ubf.erl","", ""),
    test_ubf(T).


test11() ->
    T = epp:parse_file("./unit/bug.erl","", ""),
    %% io:format("T=~p~n",[T]),
    test_ubf(T).

encode_print(X) ->
    io:format("~s~n",[ubf:encode(X)]).


bug1() ->
    T={ok,[{1,file,{"./unit/bug.erl",1}},
           {[{clause,2,[],[],[{call,3,{atom,3,decoe},[]}]}]},
           {[{clause,5,[],[],[{call,6,{atom,6,decode},[]}]}]},
           {eof,8}]},
    test_ubf(T).

bug2() ->
    T={ok,[{1,file,{"./unit/bug.erl",1}},
           {[{clause,2,[],[],[{{atom,3,decoe},[]}]}]},
           {[{clause,5,[],[],[{{atom,6,decode},[]}]}]},
           {eof,8}]},
    test_ubf(T).

test_ubf(T) ->
    B = term_to_binary(T),
    io:format("encode test #Bin=~p~n",[size(B)]),
    L = encode(T),
    %% io:format("L=~s~n",[L]),
    io:format("ubf size =~p~n",[length(L)]),
    Val = decode(L),
    case Val of
        {ok, T, _} ->
            io:format("Identical~n");
        X ->
            io:format("Differences (~p)~n", [X]),
            io:format("Val=~p~n",[T])
    end.
