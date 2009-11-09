-module(stateless_plugin).

-include("ubf.hrl").

-export([info/0, description/0, keepalive/0]).
-export([handlerStart/1, handlerStop/3, handlerRpc/1]).

-export([client_breaks_req01/0, client_timeout_req03/1]).
-export([server_breaks_req01/0, server_timeout_req03/1, server_crash_req05/0]).

%% NOTE the following two lines
-compile({parse_transform,contract_parser}).
-add_contract("./Unit-EUnit-Files/stateless_plugin").
-add_types({types_plugin, [contract_res,contract_req,description_res,description_req,info_res,info_req]}).
-add_types({types_plugin, [keepalive_res,keepalive_req]}).
-add_types({types_plugin, [timeout]}).
-add_types({types_plugin, [server_crash_res05,server_crash_req05,
                           server_timeout_res03,server_timeout_req03,
                           server_breaks_res01,server_breaks_req01,
                           client_timeout_res03,client_timeout_req03,
                           client_breaks_res01,client_breaks_req01]}).


info() ->
    "I am a stateless server".

description() ->
    "An stateless server programmed by UBF".

keepalive() ->
    ok.


%% @spec handlerStart(Args::list(any())) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args) ->
    {accept,ok,none,unused}.

%% @spec handlerStop(Pid::pid(), Reason::any(), StateData::term()) -> void()
%% @doc stop handler
handlerStop(_Pid, _Reason, _StateData) ->
    unused.

%% @spec handlerRpc(Event::any()) ->
%%          Reply::any()
%% @doc rpc handler
handlerRpc(Event)
  when Event==client_breaks_req01
       ; Event==server_breaks_req01
       ; Event==server_crash_req05
       ->
    ?MODULE:Event();
handlerRpc({Event,Timeout})
  when Event==client_timeout_req03
       ; Event==server_timeout_req03
       ->
    ?MODULE:Event(Timeout);
handlerRpc(Event)
  when Event==info; Event==description ->
    ?S(?MODULE:Event());
handlerRpc(Event)
  when Event==keepalive ->
    ?MODULE:Event();
handlerRpc(Event) ->
    {Event, not_implemented}.


%%%----------------------------------------------------------------------
%%% Implementation functions
%%%----------------------------------------------------------------------

client_breaks_req01() ->
    exit(client_breaks_req01_should_not_be_called).

client_timeout_req03(Timeout) ->
    timer:sleep(Timeout),
    client_timeout_res03.

server_breaks_req01() ->
    server_breaks_res01_with_this_response.

server_timeout_req03(Timeout) ->
    timer:sleep(Timeout),
    server_timeout_res03.

server_crash_req05() ->
    exit(server_crash_res05_with_this_response).
