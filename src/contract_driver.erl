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

%%% @doc Generic protocol driver process for protocol sessions.

-module(contract_driver).

-export([start/3, relay/3, loop/5, loop/6]).

%% Interface Functions
-ifndef(old_callbacks).

-type contract() :: module().
-type options() :: list(term()).
-type parsed_options() :: term().
-type cont_init() :: {init,Rest::term(),Extras::term()} | {more,More::term()}.
-type cont_done() :: {done,Term::term(),Rest::term(),Extras::term()} | {more,More::term()}.
-type io() :: any().

-callback start(contract()) -> pid().
-callback start(contract(), options()) -> pid().

-callback init(contract()) -> {parsed_options(), cont_init()}.
-callback init(contract(), options()) -> {parsed_options(), cont_init()}.

-callback encode(contract(), parsed_options(), term()) -> io().
-callback decode(contract(), parsed_options(), cont_init(), io()) -> cont_done().

-else. % -ifndef(old_callbacks).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start,1}
     , {start,2}
     , {init,1}
     , {init,2}
     , {encode,3}
     , {decode,4}
    ];
behaviour_info(_Other) ->
	undefined.

-endif. % -ifndef(old_callbacks).

start(Module, Contract, Options) ->
    receive
        {start, Pid, Socket} ->
            loop(Module, Contract, Options, Pid, Socket);
        stop ->
            exit(start)
    end.

relay(Module, Pid, Pid1) ->
    put('ubf_info', Module),
    Pid ! {relay, self(), Pid1},
    ok.

loop(Module, Contract, Options, Pid, Socket) ->
    loop(Module, Contract, Options, Pid, Socket, 16#ffffffff).

loop(Module, Contract, Options, Pid, Socket, Timeout) ->
    put('ubf_proto', tcp),
    put('ubf_socket', Socket),
    {ParsedOptions, Cont} = Module:init(Contract, Options),
    loop(Module, Contract, ParsedOptions, Pid, Socket, Timeout, Cont).

%% @doc Driver main loop.
%%
%% - A driver sits between a socket and a Pid
%% - Stuff on the socket is send to the Pid
%% - Stuff from the Pid is send to the socket
%% - When it is called the Socket has been set to send messages to the
%%   driver and the Pid exists
%% - If one side dies the process dies
%%

loop(Module, Contract, Options, Pid, Socket, Timeout, Cont) ->
    receive
        {Pid, {X, _}=Term} when X =:= event_in; X =:= event_out ->
            Binary = Module:encode(Contract, Options, Term),
            ok = gen_tcp:send(Socket, Binary),
            loop(Module, Contract, Options, Pid, Socket, Timeout, Cont);
        {Pid, Term} ->
            ok = inet:setopts(Socket, [{active, once}]),
            Binary = Module:encode(Contract, Options, Term),
            ok = gen_tcp:send(Socket, Binary),
            loop(Module, Contract, Options, Pid, Socket, Timeout, Cont);
        {tcp, Socket, Binary} ->
            case Module:decode(Contract, Options, Cont, Binary) of
                {done, {X, _}=Term, A, B} when X =:= event_in; X =:= event_out ->
                    ok = inet:setopts(Socket, [{active, once}]),
                    Pid ! {self(), Term},
                    Cont1 = {init, A, B},
                    loop(Module, Contract, Options, Pid, Socket, Timeout, Cont1);
                {done, Term, A, B} ->
                    Pid ! {self(), Term},
                    Cont1 = {init, A, B},
                    loop(Module, Contract, Options, Pid, Socket, Timeout, Cont1);
                {more, _}=Cont1 ->
                    ok = inet:setopts(Socket, [{active, once}]),
                    loop(Module, Contract, Options, Pid, Socket, Timeout, Cont1)
            end;
        {changeContract, Pid, Contract1} ->
            loop(Module, Contract1, Options, Pid, Socket, Timeout, Cont);
        {relay, Pid, Pid1} ->
            ok = inet:setopts(Socket, [{active, once}]),
            loop(Module, Contract, Options, Pid1, Socket, Timeout, Cont);
        {tcp_closed, Socket} ->
            exit(socket_closed);
        {tcp_error, Socket, Reason} ->
            gen_tcp:close(Socket),
            exit({socket_error,Reason});
        stop ->
            Pid ! stop,
            gen_tcp:close(Socket),
            exit(normal);
        Any ->
            io:format("~p:~p *** ~p dropping:~p~n",[Module, self(), Pid, Any]),
            loop(Module, Contract, Options, Pid, Socket, Timeout, Cont)
    after Timeout ->
            gen_tcp:close(Socket),
            exit(timeout)
    end.
