%% @doc Protocol driver process for UBF(A) protocol sessions.
%%
%% <img src="../priv/doc/ubf-flow-01.png"></img>

-module(ubf_driver).
-behaviour(contract_driver).

-export([start/1, init/1, encode/2, decode/4]).

start(Contract) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract) end, ubf_client_driver).

init(_Contract) ->
    ubf:decode_init().

encode(Contract, Term) ->
    [ubf:encode(Term, Contract), "\n"].

decode(Contract, Cont, Binary, CallBack) ->
    List = binary_to_list(Binary),
    Cont1 = ubf:decode(List, Contract, Cont),
    decode(Contract, Cont1, CallBack).

decode(_Contract, {more, _}=Cont, _CallBack) ->
    Cont;
decode(Contract, {ok, Term, List}=_Cont, CallBack) ->
    CallBack(Term),
    Cont1 = ubf:decode(List, Contract),
    decode(Contract, Cont1, CallBack).
