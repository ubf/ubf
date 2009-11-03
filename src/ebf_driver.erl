%% @doc Server-side protocol driver process for EBF (Erlang Binary
%% Format) protocol sessions.
%%
%% This driver automagically relies on the OTP `gen_tcp' "packet"
%% feature, using a 4-byte prefix to specify the size of the data
%% coming from the client.  Similarly, this packet feature is used
%% when sending our reply back to the client.
%%
%% The process executing `loop()' in this module is represented in the
%% diagram below by the "UBF Driver" circle.
%% <img src="../priv/doc/ubf-flow-01.png"></img>

-module(ebf_driver).

-export([start/0, loop/2, loop/3, relay/2]).

start() ->
    proc_utils:spawn_link_debug(fun() -> start1() end, ebf_client_driver).

start1() ->
    receive
        {start, Socket, Pid} ->
            loop(Socket, Pid);
        stop ->
            exit(start)
    end.

loop(Socket, Pid) ->
    loop(Socket, Pid, 16#ffffffff).

loop(Socket, Pid, Timeout) ->
    put('ubf_socket', Socket),
    Cont = undefined,
    loop(Socket, Pid, Timeout, Cont).

relay(Pid, Pid1) ->
    put('ubf_info', ?MODULE),
    Pid ! {relay, self(), Pid1}.

%% @doc Driver main loop.
%%
%% <ul>
%% <li> A driver sits between a socket and a Pid </li>
%% <li> Stuff on the socket is send to the Pid </li>
%% <li> Stuff from the Pid is send to the socket </li>
%% <li> When it is called the Socket has been
%% set to send messages to the driver
%% and the Pid exists  </li>
%% <li> If one side dies the process dies </li>
%% </ul>

loop(Socket, Pid, Timeout, Cont) ->
    receive
        {Pid, Term} ->
            Data = erlang:term_to_binary(Term),
            ok = gen_tcp:send(Socket, Data),
            loop(Socket, Pid, Timeout, Cont);
        stop ->
            Pid ! stop,
            gen_tcp:close(Socket),
            exit(normal);
        {changeContract, _HandlerMod1} ->
            loop(Socket, Pid, Timeout, Cont);
        {relay, _From, Pid1} ->
            loop(Socket, Pid1, Timeout, Cont);
        {tcp_closed, Socket} ->
            Pid ! stop,
            exit(socket_closed);
        {tcp_error, Socket, Reason} ->
            gen_tcp:close(Socket),
            exit({socket_error,Reason});
        {tcp, Socket, Data} ->
            T = erlang:binary_to_term(Data),
            handle_data(Socket, Pid, Timeout, {done, T});
        Any ->
            io:format("~p:~p *** ~p dropping:~p~n",[?FILE, self(), Pid, Any]),
            loop(Socket, Pid, Timeout, Cont)
    after Timeout ->
            gen_tcp:close(Socket),
            exit(timeout)
    end.

handle_data(Socket, Pid, Timeout, {done, Term}) ->
    Pid ! {self(), Term},
    Cont1 = undefined,
    loop(Socket, Pid, Timeout, Cont1).
