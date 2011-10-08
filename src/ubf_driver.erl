%% @doc Protocol driver process for UBF(A) protocol sessions.
%%
%% The process executing `loop()' in this module is represented in the
%% diagram below by the "UBF Driver" circle.
%% <img src="ubf-flow-01.png"></img>

-module(ubf_driver).
-behaviour(contract_driver).

-export([start/1, start/2, init/1, init/2, encode/3, decode/5]).

start(Contract) ->
    start(Contract, []).

start(Contract, Options) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract, Options) end, ubf_client_driver).

init(Contract) ->
    init(Contract, []).

init(_Contract, Options) ->
    Safe = safe(Options),
    {Safe, ubf:decode_init(Safe)}.

encode(Contract, _Safe, Term) ->
    [ubf:encode(Term, Contract), "\n"].

decode(Contract, Safe, Cont, Binary, CallBack) ->
    String = binary_to_list(Binary),
    Cont1 = ubf:decode(String, Contract, Cont),
    decode(Contract, Safe, Cont1, CallBack).

decode(Contract, Safe, {ok, Term, String}=_Cont, CallBack) ->
    CallBack(Term),
    Cont1 = ubf:decode_init(Safe, String),
    decode(Contract, Safe, Cont1, CallBack);
decode(_Contract, _Safe, Cont, _CallBack) ->
    Cont.

safe(Options) ->
    proplists:get_bool(safe, Options).
