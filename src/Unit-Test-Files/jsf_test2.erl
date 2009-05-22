-module(jsf_test2).

-compile(export_all).

-define(PRINTRESULT, 
    case Out of
        Exp ->
            %%io:format("~p:~p:~p:~p~n  In =~p~n  Out=~p~n", [?MODULE, Tst, Acc, Desc, In, Out]),
            ok;
        _ ->
            io:format("### FAILED:~p:~p:~p:~p~n  In =~p~n  Out=~p~n", [?MODULE, Tst, Acc, Desc, In, Out]),
            io:format("  Exp=~p~n", [Exp])
    end
).


tests() ->
    true = test_json_decode_1(),
    true = test_json_encode_1(),
    true = test_jsf_decode_1(),
    true = test_jsf_encode_1(),
    true.

%% jsf encode: ubf -> json
%%     decode: ubj <- json

test_json_decode_1() ->
    Tst = test_json_decode_1,
    lists:foldl(
        fun({Desc, In, Exp, _}, Acc) ->
            {ok, Out, []} = rfc4627:decode(In),
            ?PRINTRESULT,
            Out = Exp,
            Acc + 1
        end,
        1,
        data1()
    ),
    true.


test_json_encode_1() ->
    Tst = test_json_encode_1,
    lists:foldl(
        fun({Desc, Exp, In, _}, Acc) ->
            Out = rfc4627:encode(In),
            ?PRINTRESULT,
            Out = Exp,
            Acc + 1
        end,
        1,
        data1()
    ),
    true.

test_jsf_decode_1() ->
    Tst = test_jsf_decode_1,
    lists:foldl(
        fun({Desc, In, _, Exp}, Acc) ->
            {ok, Out, []} = jsf:decode(In, test_plugin),
            ?PRINTRESULT,
            Out = Exp,
            Acc + 1
        end,
        1,
        data1()
    ),
    true.

test_jsf_encode_1() ->
    Tst = test_jsf_encode_1,
    lists:foldl(
        fun({Desc, Exp, _, In}, Acc) ->
            Out = jsf:encode(In, test_plugin),
            ?PRINTRESULT,
            Out = Exp,
            Acc + 1
        end,
        1,
        data1()
    ),
    true.

data1() ->
    [
        %% description
        %% JSON - string()
        %% ERLANG - term()
        %% UBF - term()
        {
            "JSON true"
            , "true"
            , true
            , true
        }, {
            "JSON false"
            , "false"
            , false
            , false
        }, {
            "JSON null - UBF undefined"
            , "null"
            , null
            , undefined
        }, {
            "JSON 101"
            , "101"
            , 101
            , 101
        }, {
            "JSON 1.5"
            , "1.50000000000000000000e+00"
            , 1.5
            , 1.5
        }, {
            "JSON array"
            , "[1,2,3]"
            , [1,2,3]
            , [1,2,3]
        }, {
            "JSON object"
            , "{\"key1\":\"a\",\"key2\":2}"
            , {obj, [{"key1", <<"a">>}, {"key2", 2}] }
            , {'#P', [{<<"key1">>, <<"a">>}, {<<"key2">>, 2}] }
        }, {
            "JSON object - UBF atom"
            , "{\"$A\":\"atomname\"}"
            , {obj, [{"$A", <<"atomname">>}] }
            , atomname
        }, {
            "JSON object - UBF string"
            , "{\"$S\":\"stringval\"}"
            , {obj, [{"$S", <<"stringval">>}] }
            , {'#S', "stringval"}
        }, {
            "JSON object - UBF tuple"
            , "{\"$T\":[\"a\",2]}"
            , {obj, [{"$T", [<<"a">>, 2]}] }
            , {<<"a">>, 2}
        }, {
            "JSON object - UBF record"
            , "{\"$R\":\"dummyrecord\",\"bar\":\"2\",\"foo\":1}"
            , {obj, [{"$R", <<"dummyrecord">>}, {"bar", <<"2">>}, {"foo", 1}] }
            , {dummyrecord, 1, <<"2">>}
        }
    ].
