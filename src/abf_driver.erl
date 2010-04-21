%% @doc Protocol driver process for Avro protocol sessions.
%%
%% The process executing `loop()' in this module is represented in the
%% diagram below by the "UBF Driver" circle.
%% <img src="../priv/doc/ubf-flow-01.png"></img>

-module(abf_driver).

-export([start/1, init/1, encode/2, decode/4]).

-define(DIC_KEY, abf_avro_state).

start(Contract) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract) end, abf_client_driver).

init(_Contract) ->
    erase(),
    abf:decode_init().

encode(Contract, Term) ->
    State = get(?DIC_KEY),
    {NewState, Res} = abf:encode(Term, Contract, State),
    put(?DIC_KEY, NewState),
    Res.

decode(Contract, Cont, Binary, CallBack) ->
    State = get(?DIC_KEY),
    {NewState, Cont1} = abf:decode(Binary, Contract, Cont, State),
    put(?DIC_KEY, NewState),
    decode(Contract, Cont1, CallBack).

decode(_Contract, {more, _}=Cont, _CallBack) ->
    Cont;
decode(Contract, {ok, Term, Binary}=_Cont, CallBack) ->
    State = get(?DIC_KEY),
    CallBack(Term),
    {NewState, Cont1} = abf:decode(Binary, Contract, State),
    put(?DIC_KEY, NewState),
    decode(Contract, Cont1, CallBack).
