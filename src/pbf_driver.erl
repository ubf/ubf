%% @doc Protocol driver process for PBF (Google's Protocol Buffers
%% Format) protocol sessions.
%%
%% The process executing `loop()' in this module is represented in the
%% diagram below by the "UBF Driver" circle.
%% <img src="../priv/doc/ubf-flow-01.png"></img>

-module(pbf_driver).

-export([start/1, init/1, encode/2, decode/4]).

start(Contract) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract) end, pbf_client_driver).

init(_Contract) ->
    pbf:decode_init().

encode(Contract, Term) ->
    pbf:encode(Term, Contract).

decode(Contract, Cont, Binary, CallBack) ->
    Cont1 = pbf:decode(Binary, Contract, Cont),
    decode(Contract, Cont1, CallBack).

decode(_Contract, {more, _}=Cont, _CallBack) ->
    Cont;
decode(Contract, {ok, Term, Binary}=_Cont, CallBack) ->
    CallBack(Term),
    Cont1 = pbf:decode(Binary, Contract),
    decode(Contract, Cont1, CallBack).
