%%% The MIT License
%%%
%%% Copyright (C) 2011-2013 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Callbacks for stateful plugin implementations.

-module(ubf_plugin_stateful).

%% Interface Functions
-ifndef(old_callbacks).

-type args() :: list().
-type event() :: any().
-type reply() :: any().
-type manager() :: pid().
-type managerdata() :: term().
-type statename() :: atom().
-type statedata() :: term().
-type reason() :: any().

-callback info() -> string().
-callback description() -> string().

-callback managerStart(args()) -> {ok, managerdata()} | {error, reason()}.
-callback managerRestart(args(), pid()) -> ok | {error, reason()}.
-callback managerRpc(event(), managerdata()) -> {reply(), managerdata()}.

-callback handlerStart(args(), manager()) -> {accept, reply(), statename(), statedata()} | {reject, reason()}.
-callback handlerStop(pid(), reason(), managerdata()) -> managerdata().
-callback handlerRpc(statename(), event(), statedata(), manager()) -> {reply(), statename(), statedata()}.

-else. % -ifndef(old_callbacks).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{info,0}
     , {description,0}
     , {managerStart,1}
     , {managerRestart,2}
     , {managerRpc,2}
     , {handlerStart,2}
     , {handlerStop,3}
     , {handlerRpc,4}
    ];
behaviour_info(_Other) ->
	undefined.

-endif. % -ifndef(old_callbacks).
