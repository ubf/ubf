-module(jsf_driver).

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
    Cont = [],
    loop(Socket, Pid, Timeout, Cont, jsf).

relay(Pid, Pid1) ->
    Pid ! {relay, self(), Pid1}.

loop(Socket, Pid, Timeout, Cont, HandlerMod) ->
    %% io:format("~p:~p loop Cont=~p~n", [?FILE, self(), Cont]),
    receive
        {Pid, Term} ->
            %% io:format("~p:~p sending:~p~n",[?FILE, self(), Term]),
            Data = jsf:encode(Term, HandlerMod),
            %% io:format("~p:~p sending:~s~n",[?FILE, self(), Data]),
            gen_tcp:send(Socket, [Data,"\n"]),
            loop(Socket, Pid, Timeout, Cont, HandlerMod);
        stop ->
            %% io:format("~p:~p stop~n", [?FILE, self()]),
            gen_tcp:close(Socket),
            exit(normal);
        {changeContract, HandlerMod1} ->
            loop(Socket, Pid, Timeout, Cont, HandlerMod1);
        {relay, _From, Pid1} ->
            loop(Socket, Pid1, Timeout, Cont, HandlerMod);
        {tcp_closed, Socket} ->
            %% io:format("~p:~p closing~n",[?FILE, self()]),
            exit(socket_closed);
        {tcp_error, Socket} ->
            %% io:format("~p:~p closing~n",[?FILE, self()]),
            gen_tcp:close(Socket),
            exit(socket_error);
        {tcp, Socket, Data} ->
            T = binary_to_list(Data),
            %% io:format("~p:~p received raw=|~s|~n",[?FILE, self(), T]),
            Cont1 = jsf:decode(lists:append(Cont, T), HandlerMod),
            %% io:format("~p:~p Cont1=~p~n",[?FILE, self(), Cont1]),
            handle_data(Socket, Pid, Timeout, Cont1, HandlerMod);
        Any ->
            io:format("~p:~p *** ~p dropping:~p~n",[?FILE, self(), Pid, Any]),
            loop(Socket, Pid, Timeout, Cont, HandlerMod)
    after Timeout ->
            gen_tcp:close(Socket),
            exit(timeout)
    end.

handle_data(Socket, Pid, Timeout, {ok, Term, []}, HandlerMod) ->
    %% io:format("~p:~p sending to ~p ~p~n", [?FILE, self(), Pid, {self(),Term}]),
    Pid ! {self(), Term},
    %% io:format("~p:~p~n",[?FILE, self()]),
    loop(Socket, Pid, Timeout, [], HandlerMod);
handle_data(Socket, Pid, Timeout, {ok, Term, Str}, HandlerMod) ->
    %% io:format("~p:~p sending to ~p ~p~n", [?FILE, self(), Pid, {self(),Term}]),
    Pid ! {self(), Term},
    Cont = jsf:decode(Str, HandlerMod),
    %% io:format("~p:~p Cont=~p~n",[?FILE, self(), Cont]),
    handle_data(Socket, Pid, Timeout, Cont, HandlerMod);
handle_data(_Socket, _Pid, _Timeout, XX, _HandlerMod) ->
    io:format("~p:~p uugn:~p~n",[?FILE, self(), XX]).
