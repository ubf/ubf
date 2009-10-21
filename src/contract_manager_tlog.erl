-module(contract_manager_tlog).

-export([checkIn/3, checkOut/8, checkCallback/4]).
-export([checkOutError/4, checkOutError/5]).

checkIn(_Q, _State, _Mod) ->
    erlang:now().

checkOut(_StartTime, _Q, _State, _Mod, timeout, _NewState, _NewMod, _Status) ->
    ok; % @tbd do not log timeouts
checkOut(StartTime, Q, _State, Mod, Reply, _NewState, _NewMod, Status) ->
    %% DISABLE gmt_tlog_h:tlog_ubf(StartTime, Mod, Q, Reply, Status),
    ok.

checkCallback(_Msg, _State, _Mod, _Status) ->
    ok.

checkOutError(Q, State, Mod, Error) ->
    error_logger:warning_msg("rpc error: ~p:~p(~p): ~p\n", [Mod, State, Q, Error]),
    %% DISABLE gmt_tlog_h:tlog_ubf(undefined, Mod, Q, Error, error),
    ok.

checkOutError(StartTime, Q, State, Mod, Error) ->
    error_logger:warning_msg("rpc error: ~p:~p(~p): ~p\n", [Mod, State, Q, Error]),
    %% DISABLE gmt_tlog_h:tlog_ubf(StartTime, Mod, Q, Error, error),
    ok.
