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

-export([start/1, init/1, encode/2, decode/4]).

start(Contract) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract) end, ebf_client_driver).

init(_Contract) ->
    undefined.

encode(_Contract, Term) ->
    erlang:term_to_binary(Term).

decode(_Contract, _Cont, Binary, CallBack) ->
    Term = erlang:binary_to_term(Binary),
    CallBack(Term),
    undefined.
