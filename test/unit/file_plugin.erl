%%%
%%% @doc UBF implementation (callback module) for a simple file server.
%%%
%%% The original implementation of this module is Joe Armstrong's.
%%% All of the EDoc commentary has been added to assist beginners in
%%% understanding how to implement a UBF implementation/callback
%%% module for a very simple stateful UBF protocol.
%%%
%%% This module can be used as a template for starting a new callback
%%% module:
%%% <ol>
%%% <li> Create your protocol contract, e.g. <tt>myproto_plugin.con</tt> </li>
%%% <li> Copy this file to a new name, e.g. <tt>myproto_plugin.erl</tt>. </li>
%%% <li> Edit the <tt>-module</tt> line to use 'myproto_plugin'. </li>
%%% <li> Edit the <tt>-add_contract</tt> line to use 'myproto_plugin'. </li>
%%% <li> Change the <tt>info()</tt> and <tt>description()</tt> strings. </li>
%%% <li> Most protocol implementations do not require changes to
%%%      <tt>managerStart()</tt> <tt>managerRpc()</tt>. </li>
%%% <li> Edit <tt>handlerStart()</tt> to take care of any server-side
%%%      initialization/actions and private state required when a new
%%%      client connection is received. </li>
%%% <li> Edit <tt>handlerStop()</tt> to take care of any server-side
%%%      clean-up details when a client connection is unexpectedly
%%%      closed. </li>
%%% <li> Edit <tt>handlerRpc()</tt> to implement each of the RPC calls. </li>
%%% </ol>

-module(file_plugin).
-behavior(ubf_plugin_stateful).

-include("ubf.hrl").

%% Mandatory callback functions
-export([info/0, description/0,
         managerStart/1, handlerStop/3,
         handlerStart/2,
         managerRpc/2, handlerRpc/4]).

-import(lists, [map/2, member/2]).

%% NOTE: The -add_contract line uses a path relative to our parent
%%       dir, "..".  That's because "make" is running with its working
%%       dir in our parent dir, not this dir.

-compile({parse_transform, contract_parser}).
-add_contract("./test/unit/file_plugin").

%% @spec () -> string()
%% @doc Mandatory callback function: Return info/version string.

info() -> "I am a mini file server".

%% @spec () -> string()
%% @doc Mandatory callback function: Return description string.

description() -> "

Commands:

    'ls'$  List files
    {'get' File} => Length ~ ... ~ | noSuchFile

".

%% @spec (term()) -> {ok, term()}
%% @doc Mandatory callback function: Manager initialization function.
%%
%% The term returned here is passed to the handlerStop/3 callback if a
%% client connection fails.

managerStart(_ArgFromMetaManager) ->
    {ok, myManagerState}.

%% @spec (term(), term()) -> term()
%% @doc Mandatory callback function: Manager call handler.
%%
%% TODO: Document this callback's real purpose.
%%
%% NOTE: This function's implementation, a simple password checker, is
%% not used.

managerRpc(secret, State) ->
    {accept, welcomeToFTP, State};
managerRpc(_, State) ->
    {reject, badPassword, State}.

%% @spec (term(), pid()) -> {accept, term(), atom(), term()} | {reject, term()}
%% @doc Mandatory callback function: New UBF connection handler.
%%
%% If the handler wishes to accept the connection, it returns the tuple:
%% <ul>
%% <li> 'accept' </li>
%% <li> Reply ... a term that is returned directly to the UBF client as
%%      a response to the startSession() call. </li>
%% <li> HandlerState ... the initial UBF contract state name. </li>
%% <li> HanderData ... an arbitrary term to store this server's
%%      private connection data.  The word "state" is typically used
%%      here, but the word "data" is used instead, to avoid confusion
%%      between the "state" name of the UBF contract finite-state
%%      machine and the "state" data for the connection handler. </li>
%% </ul>

handlerStart(_ArgFromUbfClient, _ManagerPid) ->
    Reply = ?S(info()),
    HandlerState = start,
    HandlerData = myFirstData0_is_not_used,
    {accept, Reply, HandlerState, HandlerData}.

%% @spec (pid(), term(), term()) -> term()
%% @doc Mandatory callback function: a client session has been terminated.
%%
%% The return value is the new manager private data term.

handlerStop(Pid, Reason, ManagerData) ->
    io:format("Client stopped: ~p ~p ~p~n", [Pid, Reason, ManagerData]),
    ManagerData.

%% @spec (atom(), term(), term(), term()) -> {term(), atom(), term()} | tuple()
%% @doc Mandatory callback function: process a single client RPC call.
%%
%% Valid return values are:
%% <ul>
%% <li> {Reply::term(), NewStateName::atom(), NewHandlerData::term()} </li>
%% <li> {changeContract, Reply, State1, HandlerMod, State2, Data2, ManPid} </li>
%% </ul>

handlerRpc(start = StateName, ls = _Call, H_Data, _Env) ->
    io:format("ls: H_Data ~p\n", [H_Data]),
    io:format("ls: Env    ~p\n", [_Env]),
    {ok, Files} = file:list_dir("."),
    Ret = map(fun(I) -> ?S(I) end, Files),
    {{files, Ret}, StateName, H_Data};
handlerRpc(start = StateName, {get, ?S(File)} = _Call, H_Data, _Env) ->
    {ok, Files} = file:list_dir("."),
    case member(File, Files) of
        true ->
            {ok, Bin} = file:read_file(File),
            {Bin, StateName, H_Data};
        false ->
            {noSuchFile, stop, H_Data}
    end;
handlerRpc(Any, info = _Call, H_Data, _) ->
    {?S(info()), Any, H_Data};
handlerRpc(Any, description = _Call, H_Data, _Manager) ->
    {?S(description()), Any, H_Data}.
