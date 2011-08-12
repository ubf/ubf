%% @doc Protocol driver process for EBF (Erlang Binary Format)
%% protocol sessions.
%%
%% This driver automagically relies on the OTP `gen_tcp' "packet"
%% feature, using a 4-byte prefix to specify the size of the data
%% coming from the client.  Similarly, this packet feature is used
%% when sending our reply back to the client.
%%
%% The process executing `loop()' in this module is represented in the
%% diagram below by the "UBF Driver" circle.
%% <img src="../priv/doc/ubf-flow-01.png"></img>

-module(ebf_driver).
-behaviour(contract_driver).

-export([start/1, start/2, init/1, init/2, encode/3, decode/5]).

start(Contract) ->
    start(Contract, []).

start(Contract, Options) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract, Options) end, ebf_client_driver).

init(Contract) ->
    init(Contract, []).

init(_Contract, Options) ->
    {Options, undefined}.

encode(_Contract, _Options, Term) ->
    erlang:term_to_binary(Term).

decode(_Contract, Options, undefined, Binary, CallBack) ->
    Term = erlang:binary_to_term(Binary, Options),
    CallBack(Term),
    undefined.
