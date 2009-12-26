%% @doc Contract manager transaction logging.
%%
%% This module can be used by the server to log individual protocol
%% transactions, if desired.  This simple default module uses the
%% Erlang/OTP `error_logger' module; we highly recommend that the
%% `sasl' application be running to take full advantage of OTP's error
%% and event handling capabilities.

-module(contract_manager_tlog).

-export([rpcIn/3, rpcOut/8, eventOut/4]).
-export([rpcOutError/4, rpcOutError/5]).
-export([rpcFinish/1]).

-export([lpcIn/3, lpcOut/8]).
-export([lpcOutError/5]).

rpcIn(_Q, _State, _Mod) ->
    erlang:now().

rpcOut(_StartTime, Q, State, Mod, Reply, _NewState, _NewMod, Status) ->
    fun() ->
            if Status =:= server_broke_contract ->
                    error_logger:warning_msg("rpc server error: ~p:~p(~p): ~p\n", [Mod, State, Q, Reply]);
               true ->
                    noop
            end,
            ok
    end.

eventOut(_Msg, _State, _Mod, _Status) ->
    %% not supported
    ok.

rpcOutError(Q, State, Mod, Error) ->
    fun() ->
            error_logger:warning_msg("rpc error: ~p:~p(~p): ~p\n", [Mod, State, Q, Error]),
            ok
    end.

rpcOutError(_StartTime, Q, State, Mod, Error) ->
    fun() ->
            error_logger:warning_msg("rpc error: ~p:~p(~p): ~p\n", [Mod, State, Q, Error]),
            ok
    end.

rpcFinish(TLog) when is_function(TLog) ->
    TLog();
rpcFinish(TLog) ->
    TLog.

lpcIn(_Q, _State, _Mod) ->
    erlang:now().

lpcOut(_StartTime, Q, State, Mod, Reply, _NewState, _NewMod, Status) ->
    if Status =:= server_broke_contract ->
            error_logger:warning_msg("lpc server error: ~p:~p(~p): ~p\n", [Mod, State, Q, Reply]);
       true ->
            noop
    end,
    ok.

lpcOutError(_StartTime, Q, State, Mod, Error) ->
    error_logger:warning_msg("lpc error: ~p:~p(~p): ~p\n", [Mod, State, Q, Error]),
    ok.
