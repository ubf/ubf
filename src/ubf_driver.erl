%%% The MIT License
%%%
%%% Copyright (C) 2011-2015 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Protocol driver process for UBF(a) protocol sessions.

-module(ubf_driver).
-behaviour(contract_driver).

-export([start/1, start/2, init/1, init/2, encode/3, decode/4]).

start(Contract) ->
    start(Contract, []).

start(Contract, Options) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract, Options) end, ubf_client_driver).

init(Contract) ->
    init(Contract, []).

init(_Contract, Options) ->
    Safe = safe(Options),
    {Safe, ubf:decode_init(Safe)}.

encode(Contract, _Safe, Term) ->
    [ubf:encode(Term, Contract), "\n"].

decode(Contract, Safe, {init, Rest, undefined}, Binary) ->
    String = binary_to_list(Binary),
    Cont = ubf:decode_init(Safe, Rest),
    ubf:decode(String, Contract, Cont);
decode(Contract, _Safe, Cont, Binary) ->
    String = binary_to_list(Binary),
    ubf:decode(String, Contract, Cont).

safe(Options) ->
    proplists:get_bool(safe, Options).
