-module(contract_manager_tlog).

-export([checkIn/3, checkOut/8, checkCallback/4]).

checkIn(_Q, _State, _Mod) ->
    erlang:now().

checkOut(_StartTime, _Q, _State, _Mod, _Reply, _NewState, _NewMod, _Status) ->
    ok.

checkCallback(_Msg, _State, _Mod, _Status) ->
    ok.
