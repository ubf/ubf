-module(client).

%% -compile(export_all).

%% start(Host,Port)  -> {ok, Pid, Service} | {error, What}
%% rpc(Pid, Q) -> Reply
%% stop(Pid)   -> ack.

-export([connect/2, rpc/2, stop/1, install_default_handler/1, 
	 install_handler/2]).

-import(ubf_utils, [spawn_link_debug/2]).
-import(lists, [reverse/1]).

%% s(X) -> {'#S', X}.

debug(F, S) -> true.
%%debug(F, S) -> io:format(F, S).

connect(Host, Port) ->
    Self = self(),
    Pid = spawn_link_debug(client,
			   fun() -> client(Self, Host, Port)  end),
    receive
	{Pid, {ok, Service}} ->
	    debug(">>> client:start ~p ~p~n"
		  "<<< ~p~n",
		  [Host, Port , {ok, Pid, Service}]),
	    {ok, Pid, Service};
	{Pid, Error={error,_}} ->
	    debug(">>> client:start ~p ~p~n"
		  "<<< ~p~n",
		  [Host, Port , Error]),
	    Error
    end.

%% This executes inside the client

client(Parent, Host, Port) ->
    process_flag(trap_exit, true),
    case gen_tcp:connect(Host, Port, [binary, {active, true}]) of
	{ok, Socket} ->
	    %% start a driver
	    Driver = ubf_driver:start(),
	    %% get the socket to send messages to the driver
	    gen_tcp:controlling_process(Socket, Driver),
	    %% Kick off the driver
	    Driver ! {start, Socket, self()},
	    %% wait for a startup message
	    receive
		{Driver, {'ubf1.0', Service, _}} ->
		    Parent ! {self(), {ok, Service}},
		    client_loop(Driver)
	    after 10000 ->
		    Driver ! stop,
		    Parent ! {self(), {error, timeout}}
	    end;
	{error, E} ->
	    Parent ! {self(), {error, socket}}
    end.

drop_fun(Msg) ->
    io:format("GOT A CALLBACK HORRAY:~p~n",[Msg]),
    fun drop_fun/1.

stop(Pid) ->
    Pid ! stop.

rpc(Pid, Q) ->
    Pid ! {self(), {rpc, Q}},
    receive
	{Pid, Reply} ->
	    debug(">>> client RPC (~p)~n"
		  "<<< ~p ~n",[Q, Reply]),
	    Reply
    end.

install_default_handler(Pid) ->
    install_handler(Pid, fun drop_fun/1).

install_handler(Pid, Fun) ->
    Pid ! {self(), {install, Fun}},
    receive
	{Pid, Reply} ->
	    Reply
    end.

client_loop(Driver) ->
    loop(Driver, fun drop_fun/1).

loop(Driver, Fun) ->	
    receive
	stop ->
	    Driver ! stop,
	    true;
	{Driver, {event, Event}} ->
	    %% asynchronous event handler
	    Fun1 = Fun(Event),
	    loop(Driver, Fun1);
	{From, {rpc, Q}} ->
	    %% rpc
	    Driver ! {self(), Q},
	    receive
		{Driver, {R, S}} ->
		    From ! {self(), {reply, R, S}},
		    loop(Driver, Fun);
		{Driver, {error, X}} ->
		    From ! {self(), {error, X}};
		{Driver, Other} ->
		    From ! {self(), {error, Other}};
		{'EXIT', Driver, _} ->
		    wait_terminate()
	    end;
	{'EXIT', Driver, _} ->
	    wait_terminate();
	{Driver, {event, Msg, State}} ->
	    %% io:format("Dispatching callback:~p~n",[{Msg,State}]),
	    Fun1 = Fun(Msg, State),
	    loop(Driver, Fun1);
	{From, {install, Fun1}} ->
	    From ! {self(), ack},
	    loop(Driver, Fun1);
	X ->
	    io:format("*** YY Client loop dropping:~p~n",[X]),
	    loop(Driver, Fun)
    end.

wait_terminate() ->
    %% io:format("In wait termionate:~n"),
    receive
	stop ->
	    true;
	{From, {rpc, Q}} ->
	    From ! {self(), {error, stop, stop}};
	X ->
	    io:format("ZZ Client loop dropping:~p~n",[X]),
	    wait_terminate()
    end.
    






