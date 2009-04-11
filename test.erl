-module(test).

-compile(export_all).

-import(lists, [map/2, foldl/3, filter/2]).
-import(client, [rpc/2]).

defaultPort() -> 2000.

tests() ->
    ss(),
    sleep(1), %%% we have to wait if we run *immediately* there
    %% is no way of knowing if the server has started
    test(),
    true.
 
s(Str) -> {'#S', Str}.
-define(S(Str), {'#S',Str}).

    

ss() ->
    server:start(defaultPort()).

test() ->
    {ok, Pid, Name} = client:start(host(), defaultPort()),
    {ok, Info} = client:info(Pid),
    {reply, Services, _} = rpc(Pid, services),
    %% Try to start a missing  service
    rpc(Pid, {startService, s("missing"), foo}),
    %% Try to start a service with the wrong args
    rpc(Pid, {startService, {'#S', "test"}, ho}),
    %% A good start this time
    {reply, {ok, yesOffWeGo}, start} = 
	rpc(Pid, {startService, s("test"), secret}),
    {reply, ok, active} = rpc(Pid, {logon, s("hi")}),
    {reply, {files, _}, active} = rpc(Pid, ls),
    install_single_callback_handler(Pid),
    Term = {1,2,cat,rain,dogs},
    {reply, callbackOnItsWay, active} = rpc(Pid, {callback,Term}),
    {callback, Term} = expect_callback(Pid),
    {reply, yes, funny} = rpc(Pid, testAmbiguities),
    {reply, ?S("ABC"), funny} = rpc(Pid, s("abc")),
    {reply, [400,800], funny} = rpc(Pid, [200,400]),
    {reply, [194,196], funny} = rpc(Pid, "ab"),
    client:stop(Pid),
    io:format("test worked~n"),
    erlang:halt().

host() -> 
    case os:getenv("WHERE") of
	"HOME" -> "localhost";
	"WORK" ->  "p2p.sics.se"
    end.

install_single_callback_handler(Pid) ->
    Parent = self(),
    client:install_handler(Pid,
			   fun(Msg) ->
				   Parent ! {singleCallback, Msg},
				   client:install_default_handler(Pid)
			   end).

expect_callback(Pid) ->
    receive
	{singleCallback, X} ->
	    X
    end.

sleep(T) ->
    receive
    after T * 1000 ->
	    true
    end.


    
		

			  
