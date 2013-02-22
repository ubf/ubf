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

%%% @doc Generic protocol encoder/decoder for protocol sessions.
%%%
-module(contract_proto).

%% Interface Functions
-ifndef(old_callbacks).

-callback proto_vsn() -> atom().
-callback proto_driver() -> module().
-callback proto_packet_type() -> 0 | 1 | 2 | 4 | sunrm.

-else. % -ifndef(old_callbacks).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{proto_vsn,0}
     , {proto_driver,0}
     , {proto_packet_type,0}
    ];
behaviour_info(_Other) ->
	undefined.

-endif. % -ifndef(old_callbacks).
