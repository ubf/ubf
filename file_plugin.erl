-module(file_plugin).

-export([info/0, description/0,
	 managerStart/1, handlerStop/3,
	 handlerStart/2, 
	 managerRpc/2, handlerRpc/4]).

-import(lists, [map/2, member/2]).

%% NOTE the following two lines

-compile({parse_transform,contract_parser}).
-add_contract("file_plugin").

-define(S(X), {'#S',X}).
s(X) -> {'#S', X}.

%% The initial state of the manager

info() -> "I am a mini file server".

description() -> "

Commands:

    'ls'$  List files
    {'get' File} => Length ~ ... ~ | noSuchFile
    
".

managerStart(_) -> {ok, myManagerState}.

managerRpc(secret, State) ->
    {accept, welcomeToFTP, State};
managerRpc(_, State) ->
    {reject, badPassword, State}.

handlerStart(_, ManagerPid) ->
    Reply = s(info()),
    HandlerState = start,
    HandlerData = myFirstData0,
    {accept, Reply, HandlerState, HandlerData}.

handlerStop(Pid, Reason, State) ->
    io:format("Client stopped:~p ~p~n",[Pid, Reason]),
    State.

handlerRpc(start, ls, State, Env) ->
    {ok, Files} = file:list_dir("."),
    Ret = map(fun(I) -> s(I) end, Files),
    {{files, Ret}, start, State};
handlerRpc(start, {get, ?S(File)}, State, Env) ->
    {ok, Files} = file:list_dir("."),
    case member(File, Files) of
	true ->
	    {ok, Bin} = file:read_file(File),
	    {Bin, start, State};
	false ->
	    {noSuchFile, stop, State}
    end;
handlerRpc(Any, info, State, _) ->
    {s(info()), Any, State};
handlerRpc(Any, description, State, Manager) ->
    {s(description()), Any, State}.






