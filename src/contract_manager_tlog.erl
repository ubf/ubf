%% @doc Contract manager transaction logging.
%%
%% This module can be used by the server to log individual protocol
%% transactions, if desired.  This simple default module uses the
%% Erlang/OTP `error_logger' module; we highly recommend that the
%% `sasl' application be running to take full advantage of OTP's error
%% and event handling capabilities.
%%
%% tlog/6 callback API:
%%
%% -type op() :: rpc | lpc | event_in | event_out.
%% -type now() :: {pos_integer(), pos_integer(), pos_integer()}.
%% -type plugin() :: module().
%%
%% -spec tlog(op(), now(), plugin(), Q::term(), Reply::term(), Status::term()) -> ok.
%%

-module(contract_manager_tlog).

-export([rpcIn/4, rpcOut/9]).
-export([rpcOutError/5, rpcOutError/6]).
-export([rpcFinish/1]).

-export([lpcIn/4, lpcOut/9]).
-export([lpcOutError/6]).

-export([eventOut/5, eventIn/5]).

rpcIn(_TLogMod_x, _Q, _State, _Mod) ->
    erlang:now().

rpcOut(TLogMod_x, StartTime, Q, State, Mod, Reply, _NewState, _NewMod, Status) ->
    rpcOut2(tlm(TLogMod_x), StartTime, Q, State, Mod, Reply, _NewState, _NewMod, Status).

rpcOut2({TLogMod, ErrLogP}, StartTime, Q, State, Mod, Reply, _NewState, _NewMod, Status) ->
    fun() ->
            if ErrLogP, Status =:= server_broke_contract ->
                    error_logger:warning_msg("rpc server error: ~p:~p(~p): ~p\n"
                                             , [Mod, State, Q, Reply]);
               true ->
                    noop
            end,
            if TLogMod =:= undefined ->
                    ok;
               true ->
                    TLogMod:tlog(rpc, StartTime, Mod, Q, Reply, Status)
            end
    end.

rpcOutError(TLogMod_x, Q, State, Mod, Error) ->
    rpcOutError2(tlm(TLogMod_x), Q, State, Mod, Error).

rpcOutError2({TLogMod, ErrLogP}, Q, State, Mod, Error) ->
    fun() ->
            if ErrLogP ->
                    error_logger:warning_msg("rpc error: ~p:~p(~p): ~p\n"
                                             , [Mod, State, Q, Error]);
               true ->
                    noop
            end,
            if TLogMod =:= undefined ->
                    ok;
               true ->
                    TLogMod:tlog(rpc, undefined, Mod, Q, Error, error)
            end
    end.

rpcOutError(TLogMod_x, StartTime, Q, State, Mod, Error) ->
    rpcOutError2(tlm(TLogMod_x), StartTime, Q, State, Mod, Error).

rpcOutError2({TLogMod, ErrLogP}, StartTime, Q, State, Mod, Error) ->
    fun() ->
            if ErrLogP ->
                    error_logger:warning_msg("rpc error: ~p:~p(~p): ~p\n"
                                             , [Mod, State, Q, Error]);
               true ->
                    noop
            end,
            if TLogMod =:= undefined ->
                    ok;
               true ->
                    TLogMod:tlog(rpc, StartTime, Mod, Q, Error, error)
            end
    end.

rpcFinish(TLog) when is_function(TLog) ->
    TLog();
rpcFinish(TLog) ->
    TLog.

lpcIn(_TLogMod_x, _Q, _State, _Mod) ->
    erlang:now().

lpcOut(TLogMod_x, StartTime, Q, State, Mod, Reply, _NewState, _NewMod, Status) ->
    lpcOut2(tlm(TLogMod_x), StartTime, Q, State, Mod, Reply, _NewState, _NewMod, Status).

lpcOut2({TLogMod, ErrLogP}, StartTime, Q, State, Mod, Reply, _NewState, _NewMod, Status) ->
    if ErrLogP, Status =:= server_broke_contract ->
            error_logger:warning_msg("lpc server error: ~p:~p(~p): ~p\n"
                                     , [Mod, State, Q, Reply]);
       true ->
            noop
    end,
    if TLogMod =:= undefined ->
            ok;
       true ->
            TLogMod:tlog(lpc, StartTime, Mod, Q, Reply, Status)
    end.

lpcOutError(TLogMod_x, StartTime, Q, State, Mod, Error) ->
    lpcOutError2(tlm(TLogMod_x), StartTime, Q, State, Mod, Error).

lpcOutError2({TLogMod, ErrLogP}, StartTime, Q, State, Mod, Error) ->
    if ErrLogP ->
            error_logger:warning_msg("lpc error: ~p:~p(~p): ~p\n"
                                     , [Mod, State, Q, Error]);
       true ->
            noop
    end,
    if TLogMod =:= undefined ->
            ok;
       true ->
            TLogMod:tlog(lpc, StartTime, Mod, Q, Error, error)
    end.

eventOut(TLogMod_x, Msg, State, Mod, Status) ->
    eventOut2(tlm(TLogMod_x), Msg, State, Mod, Status).

eventOut2({TLogMod, ErrLogP}, Msg, State, Mod, Status) ->
    fun() ->
            if ErrLogP, Status =:= server_broke_contract ->
                    error_logger:warning_msg("event_out server error: ~p:~p: ~p\n"
                                             , [Mod, State, Msg]);
               true ->
                    noop
            end,
            if TLogMod =:= undefined ->
                    ok;
               true ->
                    TLogMod:tlog(event_out, undefined, Mod, Msg, undefined, Status)
            end
    end.

eventIn(TLogMod_x, Msg, State, Mod, Status) ->
    eventIn2(tlm(TLogMod_x), Msg, State, Mod, Status).

eventIn2({TLogMod, ErrLogP}, Msg, State, Mod, Status) ->
    fun() ->
            if ErrLogP, Status =:= client_broke_contract ->
                    error_logger:warning_msg("event_in client error: ~p:~p: ~p\n"
                                             , [Mod, State, Msg]);
               true ->
                    noop
            end,
            if TLogMod =:= undefined ->
                    ok;
               true ->
                    TLogMod:tlog(event_in, undefined, Mod, Msg, undefined, Status)
            end
    end.

%% @doc The TLogMod_x thingie coming in is either:
%%
%%  * an atom, specifying the txn log module name, and also assume the
%%  caller wants 'error_logger' calls
%%
%%  * a tuple, {atom(), boolean()}, where the atom() is the txn log
%%  module name, and the boolean() is whether or not the caller wants
%%  'error_logger' calls.
%%
%% The other design option would be to strip out all 'error_logger'
%% calls from this module and force the TLogMod callback module to do
%% it.  However, it's probably "nice" to provide a default that casual
%% users don't need to fuss with?

tlm({_, _} = X) ->
    X;
tlm(TLogMod) when is_atom(TLogMod) ->
    {TLogMod, true}.
