-module(ubf_test).

-compile(export_all).

-import(ubf, [encode/1, decode/1]).
-import(lists, [foldl/3, reverse/1, map/2, seq/2, sort/1]).

test() ->
    decode("'person' >p # {p 1479 -22} &"
	   "  {p \"bingo\" 23} & $").

test1() ->
    decode("#5&4&3&2&1&$").

test2() ->
    {L, _} = encode(abc),
    L.

test9() ->
    test_ubf({abc,"kdjhkshfkhfkhsfkhaf", [a,c,d]}).

test10() ->
    T = epp:parse_file("ubf.erl","", ""),
    test_ubf(T).


test11() ->
    T = epp:parse_file("bug.erl","", ""),
    %% io:format("T=~p~n",[T]),
    test_ubf(T).
 
encode_print(X) ->
    io:format("~s~n",[ubf:encode(X)]).


bug1() ->
    T={ok,[{1,file,{"bug.erl",1}},
	   {[{clause,2,[],[],[{call,3,{atom,3,decoe},[]}]}]},
	   {[{clause,5,[],[],[{call,6,{atom,6,decode},[]}]}]},
	   {eof,8}]},
    test_ubf(T).

bug2() ->
    T={ok,[{1,file,{"bug.erl",1}},
	   {[{clause,2,[],[],[{{atom,3,decoe},[]}]}]},
	   {[{clause,5,[],[],[{{atom,6,decode},[]}]}]},
	   {eof,8}]},
    test_ubf(T).

test_ubf(T) ->
    B = term_to_binary(T),
    io:format("encode test #Bin=~p~n",[size(B)]),
    {L, _} = encode(T),
    %% io:format("L=~s~n",[L]),
    io:format("ubf size =~p~n",[length(L)]),
    Val = decode(L),
    case Val of 
	{done, T, _} ->
	    io:format("Identical~n");
	_ ->
	    io:format("Differences~n"),
	    io:format("Val=~p~n",[T])
    end.



