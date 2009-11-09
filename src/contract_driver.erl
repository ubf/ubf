%% @doc Generic protocol driver process for protocol sessions.
%%
%% <img src="../priv/doc/ubf-flow-01.png"></img>

-module(contract_driver).

-export([start/2, relay/3, loop/4, loop/5, loop/6]).

start(Module, Contract) ->
    receive
        {start, Pid, Socket} ->
            loop(Module, Contract, Pid, Socket);
        stop ->
            exit(start)
    end.

relay(Module, Pid, Pid1) ->
    put('ubf_info', Module),
    Pid ! {relay, self(), Pid1}.

loop(Module, Contract, Pid, Socket) ->
    loop(Module, Contract, Pid, Socket, 16#ffffffff).

loop(Module, Contract, Pid, Socket, Timeout) ->
    put('ubf_socket', Socket),
    Cont = Module:init(Contract),
    loop(Module, Contract, Pid, Socket, Timeout, Cont).

%% @doc Driver main loop.
%%
%% <ul>
%% <li> A driver sits between a socket and a Pid </li>
%% <li> Stuff on the socket is send to the Pid </li>
%% <li> Stuff from the Pid is send to the socket </li>
%% <li> When it is called the Socket has been set to send messages to
%% the driver and the Pid exists </li>
%% <li> If one side dies the process dies </li>
%% </ul>

loop(Module, Contract, Pid, Socket, Timeout, Cont) ->
    receive
        {Pid, Term} ->
            Binary = Module:encode(Contract, Term),
            ok = gen_tcp:send(Socket, Binary),
            loop(Module, Contract, Pid, Socket, Timeout, Cont);
        {tcp, Socket, Binary} ->
            Cont1 = Module:decode(Contract, Cont, Binary, fun(Term) -> Pid ! {self(), Term} end),
            loop(Module, Contract, Pid, Socket, Timeout, Cont1);
        {changeContract, Contract1} ->
            loop(Module, Contract1, Pid, Socket, Timeout, Cont);
        {relay, _From, Pid1} ->
            loop(Module, Contract, Pid1, Socket, Timeout, Cont);
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
            loop(Module, Contract, Pid, Socket, Timeout, Cont)
    after Timeout ->
            gen_tcp:close(Socket),
            exit(timeout)
    end.
