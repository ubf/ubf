%%% -*- mode: erlang -*-
%%% @doc Sample BERT-RPC plugin.
%%%
%%%

-module(ubf_bertrpc_plugin).
-behavior(ubf_plugin_stateless).

%% Required (except keepalive/0) callback API for UBF stateless
%% implementations.
-export([info/0, description/0, keepalive/0]).
-export([handlerStart/1, handlerStop/3, handlerRpc/1, handlerEvent/1]).

-import(ubf_plugin_handler, [sendEvent/2, install_handler/2]).

-compile({parse_transform,contract_parser}).
-add_contract("ubf_bertrpc_plugin").

-include("ubf.hrl").
-include("ubf_plugin_stateless.hrl").

info() ->
    "I am a BERT-RPC server".

description() ->
    "A BERT-RPC server programmed by UBF".

keepalive() ->
    ok.

%% @doc start handler
handlerStart(_Args) ->
    ack = install_handler(self(), fun handlerEvent/1),
    {accept,ok,none,unused}.

%% @doc stop handler
handlerStop(_Pid, _Reason, _StateData) ->
    unused.

%% @doc rpc handler
%% @TODO Implement BERT-RPC 1.0 synchronous events
handlerRpc(Event) when Event==info; Event==description ->
    ?S(?MODULE:Event());
handlerRpc(Event) when Event==keepalive ->
    ?MODULE:Event().

%% @doc event handler
%% @TODO: Implement BERT-RPC 1.0 asynchronous events
handlerEvent(Event) ->
    %% Let's fake it and echo the request
    sendEvent(self(), Event),
    fun handlerEvent/1.
