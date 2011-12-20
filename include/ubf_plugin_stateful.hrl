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

%%%----------------------------------------------------------------------
%%% Description: UBF Plugin Stateful Utilities
%%%----------------------------------------------------------------------

-ifndef(ubf_plugin_stateful).
-define(ubf_plugin_stateful, true).

%%%-------------------------------------------------------------------
%%% Specs/Types
%%%-------------------------------------------------------------------

%% common callback API
-spec info() -> string().
-spec description() -> string().
-spec handlerStop(Handler::pid(), Reason::term(), StateData::term()) ->
                  NewStateData::term().

%% stateful callback API
-spec handlerStart(Args::term(), Manager::pid()) ->
                  {accept, Reply::term(), StateName::atom(), StateDate::term()} |
                  {reject, Reply::term()}.
-spec handlerRpc(StateName::atom(), Call::term(), StateDate::term(), Manager::pid()) ->
                {Reply::term(), NewStateName::atom(), NewStateData::term()}.

-spec managerStart(Args::term()) ->
                   {ok, ManagerData::term()}.
-spec managerRestart(Args::term(), Manager::pid()) ->
                     {ok, ManagerData::term()} | {error, Reason::term()}.
-spec managerRpc(Args::term(), ManagerData::term()) ->
                 {ok, NewManagerData::term()} | {error, Reason::term()}.

-endif. % -ifndef(ubf_plugin_stateful)
