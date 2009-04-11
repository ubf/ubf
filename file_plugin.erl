-module(file_plugin).

-export([manager_start/2, client_stop/3,
	 managerStart/0, handlerStartState/0, 
	 manager_rpc/2, handle_rpc/4]).

-import(lists, [map/2, member/2]).

%% NOTE the following two lines

-compile({parse_transform,contract_parser}).
-add_contract("file_plugin").

-define(S(X), {'#S',X}).
s(X) -> {'#S', X}.

%% The initial state of the manager

managerStart() -> myManagerState.

handlerStartState() -> myHandlerState.
    
%% manager_start(Args, State) is called every time a session is started
%% Args comes from client:start(Host,Port,Service,Args)
%% Service in the rcp:start must match name()
%% manager_start(Args, State) -> {accept, Reply, State} | {reject, Why, State}
%% State is the manager state.

manager_start(_, ManagerState) ->
    Reply = yesOffWeGo,
    HandlerState = start,
    HandlerData = myInitailData0,
    {accept, Reply, HandlerState, HandlerData, ManagerState}.

client_stop(Pid, Reason, State) ->
    io:format("Client stopped:~p ~p~n",[Pid, Reason]),
    State.

manager_rpc(secret, State) ->
    {accept, welcomeToFTP, State};
manager_rpc(_, State) ->
    {reject, badPassword, State}.

handle_rpc(start, ls, State, Env) ->
    {ok, Files} = file:list_dir("."),
    Ret = map(fun(I) -> s(I) end, Files),
    {{files, Ret}, start, State};
handle_rpc(start, {get, ?S(File)}, State, Env) ->
    {ok, Files} = file:list_dir("."),
    case member(File, Files) of
	true ->
	    {ok, Bin} = file:read_file(File),
	    {Bin, start, State};
	false ->
	    {noSuchFile, stop, State}
    end.




