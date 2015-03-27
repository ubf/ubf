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

%%% @doc Low-level functions for encoding and decoding the UBF(a)
%%% protocol for EBF.
%%%

-module(ebf).
-behaviour(contract_proto).

-export([proto_vsn/0, proto_driver/0, proto_packet_type/0]).
-export([encode/1, encode/2]).
-export([decode_init/0, decode_init/1, decode_init/2, decode/1, decode/2, decode/3]).


%%---------------------------------------------------------------------
proto_vsn()         -> 'ebf1.0'.
proto_driver()      -> ebf_driver.
proto_packet_type() -> 4.


%%---------------------------------------------------------------------
-spec encode(term()) -> no_return().
encode(X) ->
    encode(X, ?MODULE).

-spec encode(term(), module()) -> no_return().
encode(_X, _Mod) ->
    %% see ebf_driver.erl
    exit(notimplemented).


%%---------------------------------------------------------------------
-spec decode(binary()) -> no_return().
decode(X) ->
    decode(X, ?MODULE).

-spec decode(binary(), module()) -> no_return().
decode(X, Mod) ->
    decode(X, Mod, decode_init()).

-spec decode(binary(), module(), term()) -> no_return().
decode(_X, _Mod, _Cont) ->
    %% see ebf_driver.erl
    exit(notimplemented).

-spec decode_init() -> no_return().
decode_init() ->
    decode_init(false).

-spec decode_init(boolean()) -> no_return().
decode_init(Safe) ->
    decode_init(Safe, <<>>).

-spec decode_init(boolean(), binary()) -> no_return().
decode_init(_Safe, _Binary) ->
    %% see ebf_driver.erl
    exit(notimplemented).
