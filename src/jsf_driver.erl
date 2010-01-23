%% @doc Protocol driver process for JSF (JavaScript Format) protocol
%% sessions.
%%
%% The process executing `loop()' in this module is represented in the
%% diagram below by the "UBF Driver" circle.
%% <img src="../priv/doc/ubf-flow-01.png"></img>

-module(jsf_driver).

-export([start/1, init/1, encode/2, decode/4]).

start(Contract) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract) end, jsf_client_driver).

init(_Contract) ->
    jsf:decode_init().

encode(Contract, Term) ->
    [jsf:encode(Term, Contract), "\n"].

decode(Contract, Cont, Binary, CallBack) ->
    List = binary_to_list(Binary),
    Cont1 = jsf:decode(List, Contract, Cont),
    decode(Contract, Cont1, CallBack).

decode(_Contract, {more, _}=Cont, _CallBack) ->
    Cont;
decode(Contract, {ok, Term, List}=_Cont, CallBack) ->
    CallBack(Term),
    Cont1 = jsf:decode(List, Contract),
    decode(Contract, Cont1, CallBack).
