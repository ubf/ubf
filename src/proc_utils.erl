%% The MIT License
%%%
%%% Copyright (C) 2011-2012 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Low-level utilities for proc_socket_server.

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
