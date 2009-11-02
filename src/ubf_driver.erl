%% @doc Server-side protocol driver process for UBF(A) protocol
%% sessions.
%%
%% <img src="../priv/doc/ubf-flow-01.png"></img>

-module(ubf_driver).

-export([start/0, loop/2, loop/3, relay/2]).

start() ->
    proc_utils:spawn_link_debug(fun() -> start1() end, ubf_client_driver).

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
    Cont = ubf:decode_init(),
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
            Data = ubf:encode(Term),
            ok = gen_tcp:send(Socket, [Data,"\n"]),
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
            T = binary_to_list(Data),
            Cont1 = ubf:decode(T, Cont),
            handle_data(Socket, Pid, Timeout, Cont1);
        Any ->
            io:format("~p:~p *** ~p dropping:~p~n",[?FILE, self(), Pid, Any]),
            loop(Socket, Pid, Timeout, Cont)
    after Timeout ->
            gen_tcp:close(Socket),
            exit(timeout)
    end.

handle_data(Socket, Pid, Timeout, Cont1 = {more, _}) ->
    loop(Socket, Pid, Timeout, Cont1);
handle_data(Socket, Pid, Timeout, {done, Term, Str1}) ->
    Pid ! {self(), Term},
    Cont1 = ubf:decode(Str1),
    handle_data(Socket, Pid, Timeout, Cont1);
handle_data(_Socket, _Pid, _Timeout, XX) ->
    io:format("~p:~p uugn:~p~n",[?FILE, self(), XX]).
