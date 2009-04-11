-module(jsf_test).

-compile(export_all).

-import(jsf, [encode/1, decode/1]).
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
    L = "{\"$A\":\"abc\"}",
    L.

test9() ->
    test_jsf({abc,"kdjhkshfkhfkhsfkhaf", [a,c,d]}).

test10() ->
    T = epp:parse_file("jsf.erl","", ""),
    test_jsf(T).


test11() ->
    T = epp:parse_file("./Unit-Test-Files/bug.erl","", ""),
    %% io:format("T=~p~n",[T]),
    test_jsf(T).

encode_print(X) ->
    io:format("~s~n",[jsf:encode(X)]).


bug1() ->
    T={ok,[{1,file,{"./Unit-Test-Files/bug.erl",1}},
           {[{clause,2,[],[],[{call,3,{atom,3,decoe},[]}]}]},
           {[{clause,5,[],[],[{call,6,{atom,6,decode},[]}]}]},
           {eof,8}]},
    test_jsf(T).

bug2() ->
    T={ok,[{1,file,{"./Unit-Test-Files/bug.erl",1}},
           {[{clause,2,[],[],[{{atom,3,decoe},[]}]}]},
           {[{clause,5,[],[],[{{atom,6,decode},[]}]}]},
           {eof,8}]},
    test_jsf(T).

test_jsf(T) ->
    B = term_to_binary(T),
    io:format("encode test #Bin=~p~n",[size(B)]),
    L = encode(T),
    %%io:format("L=~p~n",[L]),
    io:format("jsf size =~p~n",[length(L)]),
    Val = decode(L),
    case Val of
        {ok, T, _} ->
            io:format("Identical~n");
        X ->
            io:format("Differences (~p)~n", [X]),
            io:format("Val=~p~n",[T])
    end.
