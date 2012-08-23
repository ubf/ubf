%%% The MIT License
%%%
%%% Copyright (C) 2011-2012 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Callbacks for stateless plugin implementations.

-module(ubf_plugin_stateless).

%% Interface Functions
-ifndef(old_callbacks).

-type args() :: list().
-type event() :: any().
-type reply() :: any().
-type statename() :: atom().
-type statedata() :: term().
-type reason() :: any().

-callback info() -> string().
-callback description() -> string().

-callback moduleStart(args()) -> any().
-callback moduleRestart(args()) -> any().

-callback handlerStart(args()) -> {accept, reply(), statename(), statedata()} | {reject, reason()}.
-callback handlerStop(pid(), reason(), statedata()) -> any().
-callback handlerRpc(event()) -> reply().

-else. % -ifndef(old_callbacks).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{info,0}
     , {description,0}
     , {moduleStart,1}
     , {moduleRestart,1}
     , {handlerStart,1}
     , {handlerStop,3}
     , {handlerRpc,1}
    ];
behaviour_info(_Other) ->
	undefined.

-endif. % -ifndef(old_callbacks).
