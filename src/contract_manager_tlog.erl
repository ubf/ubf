%%% The MIT License
%%%
%%% Copyright (C) 2011 by Joseph Wayne Norton <norton@alum.mit.edu>
%%% Copyright (C) 2002 by Joe Armstrong
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

%%% @doc Contract manager transaction logging.
%%%
%%% This module can be used by the server to log individual protocol
%%% transactions, if desired.  This simple default module uses the
%%% Erlang/OTP +error_logger+ module; we highly recommend that the
%%% +sasl+ application be running to take full advantage of OTP\'s
%%% error and event handling capabilities.

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
%% - an atom, specifying the txn log module name, and also assume the
%%   caller wants +error_logger+ calls
%%
%% - a tuple, +{atom(), boolean()}+, where the +atom()+ is the txn log
%%   module name, and the +boolean()+ is whether or not the caller
%%   wants +error_logger+ calls.
%%
%% The other design option would be to strip out all +error_logger+
%% calls from this module and force the TLogMod callback module to do
%% it.  However, it\'s probably "nice" to provide a default that
%% casual users don\'t need to fuss with?

tlm({_, _} = X) ->
    X;
tlm(TLogMod) when is_atom(TLogMod) ->
    {TLogMod, true}.
