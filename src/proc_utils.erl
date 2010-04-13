%% @doc Low-level utilities for proc_socket_server.

-module(proc_utils).

-export([spawn_link_debug/2, spawn_link_opt_debug/3, debug/0, debug/1]).

spawn_link_debug(Fun, Term) ->
    erlang:spawn_link(fun() ->
                              put('ubf_info', Term),
                              Fun()
                      end).

%% SpawnOpts should be an empty list or a list containing
%% options fullsweep_after and/or min_heap_size.
spawn_link_opt_debug(Fun, SpawnOpts, Term) ->
    erlang:spawn_opt(fun() ->
                              put('ubf_info', Term),
                              Fun()
                      end, [link | SpawnOpts]).

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
                                        UBFInfo = proplists:get_value('ubf_info', Dict),
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
