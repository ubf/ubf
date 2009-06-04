-module(stateless_plugin).

-export([handlerStart/1, handlerStop/3, handlerDispatch/2]).
-export([info/0, description/0, keepalive/0]).

-export([client_breaks_req1/0, server_breaks_req2/0, client_breaks_req3/0, server_breaks_req4/0]).
-export([dummy_req5/0, dummy_req6/0]).
-export([timeout_req7/0, timeout_req8/0]).

-include("ubf.hrl").

%% NOTE the following two lines
-compile({parse_transform,contract_parser}).
-add_contract("./Unit-EUnit-Files/stateless_plugin").
-add_types({types_plugin, [contract_res,contract_req,description_res,description_req,info_res,info_req]}).
-add_types({types_plugin, [keepalive_res,keepalive_req]}).
-add_types({types_plugin, [timeout_res8,timeout_req8,timeout_res7,timeout_req7,
                                dummy_res6,dummy_req6,dummy_res5,dummy_req5,
                                server_breaks_res4,server_breaks_req4,
                                client_breaks_res3,client_breaks_req3,
                                server_breaks_res2,server_breaks_req2,
                                client_breaks_res1,client_breaks_req1]}).

info() ->
    "I am a stateless server".

description() ->
    "An stateless server programmed by UBF".

%% @spec handlerStart(Args::list(any())) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args) ->
    {accept,ok,none,undefined}.

%% @spec handlerStop(undefined, Reason::any(), StateData::term()) -> void()
%% @doc stop handler
handlerStop(undefined, _Reason, _StateData) ->
    unused.

%% @spec handlerDispatch(DispatchKey::term(), StateData::term()) ->
%%          {term(), atom(), term()} | term()
%% @doc dispatch handler, returns {Module, Function, NewStateData} | NewStateData
handlerDispatch(keepalive, StateData) ->
    {?MODULE, keepalive, StateData};
handlerDispatch(X, StateData)
  when X==client_breaks_req1
       , X==server_breaks_req2
       , X==client_breaks_req3
       , X==server_breaks_req4
       , X==dummy_req5
       , X==dummy_req6
       , X==timeout_req7
       , X==timeout_req8
       %% NOT IMPLEMENTED, X==not_implemented_req9
       ->
    {?MODULE, X, StateData};
handlerDispatch(_X, StateData) ->
    StateData.

%% keepalive
keepalive() ->
    ok.

client_breaks_req1() ->
    exit(this_should_not_be_called).

server_breaks_req2() ->
    server_breaks_req2.

client_breaks_req3() ->
    exit(this_should_not_be_called).

server_breaks_req4() ->
    server_breaks_req4.

dummy_req5() ->
    dummy_res5.

dummy_req6() ->
    dummy_req6.

timeout_req7() ->
    receive
        this_will_never_yield ->
            exit(this_should_not_be_called)
    end.

timeout_req8() ->
    receive
        this_will_never_yield ->
            exit(this_should_not_be_called)
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
