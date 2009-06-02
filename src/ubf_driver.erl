-module(ubf_driver).

-export([start/0, loop/2, loop/3, relay/2]).

%% A driver sits between a socket and a Pid
%% Stuff on the socket is send to the Pid
%% Stuff from the Pid is send to the socket
%% The cache is cleared after every completed message
%% When it is called the Socket has been
%% set to send messages to the driver
%% and the Pid exists
%% If one side dies the process dies

start() ->
    spawn_link(fun() -> start1() end).

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
    Cont = ubf:decode_init(),
    loop(Socket, Pid, Timeout, Cont).

relay(Pid, Pid1) ->
    Pid ! {relay, self(), Pid1}.

loop(Socket, Pid, Timeout, Cont) ->
    receive
        {Pid, Term} ->
            Data = ubf:encode(Term),
            gen_tcp:send(Socket, [Data,"\n"]),
            loop(Socket, Pid, Timeout, Cont);
        stop ->
            Pid ! stop,
            gen_tcp:close(Socket);
        {changeContract, _HandlerMod1} ->
            loop(Socket, Pid, Timeout, Cont);
        {relay, _From, Pid1} ->
            loop(Socket, Pid1, Timeout, Cont);
        {tcp_closed, Socket} ->
            exit(socket_closed);
        {tcp_error, Socket} ->
            gen_tcp:close(Socket),
            exit(socket_error);
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
