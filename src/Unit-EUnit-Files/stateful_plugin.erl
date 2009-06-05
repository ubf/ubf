-module(stateful_plugin).

-export([handlerStart/2, handlerRpc/4, handlerStop/3,
         managerStart/1, managerRestart/2, managerRpc/2]).

-import(ubf_server, [ask_manager/2]).

-include("ubf.hrl").

%% NOTE the following two lines
-compile({parse_transform,contract_parser}).
-add_contract("./Unit-EUnit-Files/stateful_plugin").
-add_types({types_plugin, [contract_res,contract_req,description_res,description_req,info_res,info_req]}).
-add_types({types_plugin, [keepalive_res,keepalive_req]}).
-add_types({types_plugin, [timeout]}).
-add_types({types_plugin, [server_crash_res05,server_crash_req05,
                           server_timeout_res03,server_timeout_req03,
                           server_breaks_res01,server_breaks_req01,
                           client_timeout_res03,client_timeout_req03,
                           client_breaks_res01,client_breaks_req01]}).
-add_types({types_plugin, [restart_res,restart_req,server_crash_res06,
                           server_crash_req06,server_timeout_res04,
                           server_timeout_req04,server_breaks_res02,
                           server_breaks_req02,client_timeout_res04,
                           client_timeout_req04,client_breaks_res02,
                           client_breaks_req02]}).


%% records

%% managerState
-record(managerState, {
         }).

%% state
-record(state, {
         }).

info() ->
    "I am a stateful server".

description() ->
    "An stateful server programmed by UBF".

%% @spec managerStart(Args::list(any())) ->
%%          {ok, ManagerStateData::term()} | {error, Reason::any()}
%% @doc start manager
managerStart(_) ->
    ManagerStateData = #managerState{},
    {ok,ManagerStateData}.

%% @spec managerRestart(Args::list(any()), ManagerPid::pid()) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc restart manager
managerRestart(Args,ManagerPid) ->
    ask_manager(ManagerPid,{restartManager, Args}).

%% @spec handlerStart(Args::list(any()), ManagerPid::pid()) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args,_ManagerPid) ->
    StateData = #state{},
    {accept,ok,start,StateData}.

%% @spec handlerStop(Pid::pid(), Reason::any(), ManagerStateData::term()) -> NewManagerStateData::term()
%% @doc stop handler event
handlerStop(_Pid,_Reason,ManagerStateData)
  when is_record(ManagerStateData,managerState) ->
    ManagerStateData.

%% @spec managerRpc(Event::any(), ManagerStateData::term()) ->
%%          {ok | {ok,term()} | error | {error, Reason::any()}, NewManagerStateData::term()}
%% @doc rpc manager
managerRpc({restartManager,Args},ManagerStateData)
  when is_record(ManagerStateData,managerState) ->
    managerStart(Args).

%% @spec handlerRpc(StateName::atom(), {Event::any(),FSM::any()}, StateData::term(), ManagerPid::pid()) ->
%%          {Reply::any(), NextStateName::atom(), NewStateData::term()}
%% @doc rpc handler

%% any - real
handlerRpc(AnyStateName,info,StateData,_ManagerPid) ->
    {?S(info()),AnyStateName,StateData};
handlerRpc(AnyStateName,description,StateData,_ManagerPid) ->
    {?S(description()),AnyStateName,StateData};
handlerRpc(AnyStateName,keepalive,StateData,_ManagerPid) ->
    {keepalive(),AnyStateName,StateData}.

%% keepalive
keepalive() ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
