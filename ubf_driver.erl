-module(ubf_driver).

-export([start/0, loop/2, relay/2]).

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
    put('$ubfinfo', {ubfdriver, client}),
    receive 
	{start, Socket, Pid} ->
	    loop(Socket, Pid)
    end.

loop(Socket, Pid) ->
    %% io:format("ubf driver starting Socket=~p Pid=~p~n",[Socket,Pid]),
    Cont = ubf:decode_init(),
    loop(Socket, Pid, Cont).

relay(Pid, Pid1) ->
    Pid ! {relay, self(), Pid1}.

loop(Socket, Pid, Cont) ->
    %% io:format("loop Cont=~p~n", [Cont]),
    receive
	{Pid, Term} ->
	    %% io:format("ubf_driver sending:~p~n",[Term]),
	    Data = ubf:encode(Term),
	    %% io:format("ubf_driver sending:~s~n",[Data]),
	    gen_tcp:send(Socket, Data),
	    loop(Socket, Pid, Cont);
    	stop ->
	    %% io:format("ubf driver stop~n"),
	    gen_tcp:close(Socket),
	    exit(normal);
	{relay, From, Pid1} ->
	    loop(Socket, Pid1, Cont);
	{tcp_closed, Socket} ->
	    %% io:format("ubf_driver closing~n",[]),
	    exit(socket_closed);
	{tcp, Socket, Data} ->
	    T = binary_to_list(Data),
	    %% io:format("ubf driver received raw=|~s|~n",[T]),
	    Cont1 = ubf:decode(T, Cont),
	    %% io:format("Cont1=~p~n",[Cont1]),
	    handle_data(Socket, Pid, Cont1);
	Any ->
	    io:format("*** ubf_driver (Pid=~p) dropping:~p~n",[Pid, Any]),
	    loop(Socket, Pid, Cont)
    end.

handle_data(Socket, Pid, Cont1 = {more, _}) ->
    loop(Socket, Pid, Cont1);
handle_data(Socket, Pid, {done, Term, Str1}) ->
    %% io:format("ubf driver sending to ~p ~p~n", [Pid, {self(),Term}]),
    Pid ! {self(), Term},
    %% io:format("Str1=~p~n",[Str1]),
    Cont1 = ubf:decode(Str1),
    handle_data(Socket, Pid, Cont1);
handle_data(Socket, Pid, XX) ->
    io:format("uugn:~p~n",[XX]).










