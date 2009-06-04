
-module(proc_utils).

-export([spawn_link_debug/2, debug/0, debug/1]).

spawn_link_debug(Fun, Term) ->
    proc_lib:spawn_link(fun() ->
                                put('$ubfinfo', Term),
                                Fun()
                        end).

debug() ->
    debug(undefined).

debug(X) ->
    P = erlang:processes(),
    lists:foldl(fun(I, A) ->
                        case erlang:process_info(I) of
                            undefined ->
                                A;
                            Info ->
                                case proplists:get_value(dictionary, Info) of
                                    undefined ->
                                        A;
                                    Dict ->
                                        UBFInfo = proplists:get_value('$ubfinfo', Dict),
                                        if UBFInfo /= undefined ->
                                                if X == undefined ->
                                                        [{I,Info}|A];
                                                   X == UBFInfo ->
                                                        [{I,Info}|A];
                                                   true ->
                                                        A
                                                end;
                                           true ->
                                                A
                                        end
                                end
                        end
                end, [], P).
