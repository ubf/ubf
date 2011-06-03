%% @doc Low-level functions for encoding and decoding the UBF(A)
%% protocol for EBF.
%%

-module(ebf).
-behaviour(contract_proto).

-export([proto_vsn/0, proto_driver/0, proto_packet_type/0]).
-export([encode/1, encode/2]).
-export([decode_init/0, decode/1, decode/2, decode/3]).


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
    %% see ebf_driver.erl
    exit(notimplemented).
