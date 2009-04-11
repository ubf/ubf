-module(ebf_driver).

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
    put('$ubfinfo', {ubfdriver, ubf_client}),
    receive
        {start, Socket, Pid} ->
            loop(Socket, Pid)
    end.

loop(Socket, Pid) ->
    loop(Socket, Pid, 16#ffffffff).

loop(Socket, Pid, Timeout) ->
    %% io:format("~p:~p starting Socket=~p Pid=~p~n", [Socket,Pid]),
    Cont = undefined,
    loop(Socket, Pid, Timeout, Cont).

relay(Pid, Pid1) ->
    Pid ! {relay, self(), Pid1}.

loop(Socket, Pid, Timeout, Cont) ->
    %% io:format("~p:~p loop Cont=~p~n", [?FILE, self(), Cont]),
    receive
        {Pid, Term} ->
            %% io:format("~p:~p sending:~p~n",[?FILE, self(), Term]),
            Data = erlang:term_to_binary(Term),
            %% io:format("~p:~p sending:~s~n",[?FILE, self(), Data]),
            gen_tcp:send(Socket, Data),
            loop(Socket, Pid, Timeout, Cont);
        stop ->
            %% io:format("~p:~p stop~n", [?FILE, self()]),
            gen_tcp:close(Socket),
            exit(normal);
        {relay, _From, Pid1} ->
            loop(Socket, Pid1, Timeout, Cont);
        {tcp_closed, Socket} ->
            %% io:format("~p:~p closing~n",[?FILE, self()]),
            exit(socket_closed);
        {tcp_error, Socket} ->
            %% io:format("~p:~p closing~n",[?FILE, self()]),
            gen_tcp:close(Socket),
            exit(socket_error);
        {tcp, Socket, Data} ->
            T = erlang:binary_to_term(Data),
            %% io:format("~p:~p received raw=|~s|~n",[?FILE, self(), T]),
            handle_data(Socket, Pid, Timeout, {done, T});
        Any ->
            io:format("~p:~p *** ~p dropping:~p~n",[?FILE, self(), Pid, Any]),
            loop(Socket, Pid, Timeout, Cont)
    after Timeout ->
            gen_tcp:close(Socket),
            exit(timeout)
    end.

handle_data(Socket, Pid, Timeout, {done, Term}) ->
    %% io:format("~p:~p sending to ~p ~p~n", [?FILE, self(), Pid, {self(),Term}]),
    Pid ! {self(), Term},
    Cont1 = undefined,
    loop(Socket, Pid, Timeout, Cont1).
