%% @doc Functions for Google's Protocol Buffers&lt;->Erlang data conversion.
%%
%% For most purposes, these functions are not called by code outside
%% of this library: Erlang client &amp; Erlang server application code
%% usually have no need to use these functions.
%%
%% == Links ==
%%
%% <ul>
%% <li> http://code.google.com/p/protobuf/ </li>
%% </ul>
%%
%% @TODO add types and mapping specification
%%
%%

-module(pbf).

-include("ubf.hrl").

-export([encode_print/1, encode_print/2, encode/1, encode/2]).
-export([decode_print/1, decode_print/2, decode_init/0, decode/1, decode/2, decode/3]).

-export([atom_to_binary/1]).
-export([binary_to_existing_atom/1]).

%% Dummy hack/kludge.
-export([contract_records/0]).

contract_records() ->
    [].


%%
%%---------------------------------------------------------------------
%%
-spec encode_print(Input::term()) -> ok | no_return().
-spec encode_print(Input::term(), module()) -> ok | no_return().

-spec encode(Input::term()) -> iolist() | no_return().
-spec encode(Input::term(), module()) -> iolist() | no_return().

-spec decode_print(Input::binary()) -> ok | no_return().
-spec decode_print(Input::binary(), module()) -> ok | no_return().

-type ok() :: {ok, Output::term(), Remainder::binary()}.
-type error() :: {error, Reason::term()}.
-type cont() :: {more, fun()}.

-spec decode_init() -> cont().
-spec decode(Input::binary()) -> ok() | error() | cont().
-spec decode(Input::binary(), module()) -> ok() | error() | cont().
-spec decode(Input::binary(), module(), cont()) -> ok() | error() | cont().


-record(state,
        {
          x        % current binary to be decoded
          , stack  % current stack
          , type   % current type (optional)
          , size   % current size (optional)
          , mod    % contract
        }
       ).


%%
%%---------------------------------------------------------------------
%%
encode_print(X) ->
    encode_print(X, ?MODULE).

encode_print(X, Mod) ->
    io:format("~p~n", [encode(X, Mod)]).

encode(X) ->
    encode(X, ?MODULE).

encode(_X, _Mod) ->
    %% @TODO
    exit(unimplemented).


%%
%%---------------------------------------------------------------------
%%
decode_print(X) ->
    decode_print(X, ?MODULE).

decode_print(X, Mod) ->
    io:format("~p~n", [decode(X, Mod)]).

decode(X) ->
    decode(X, ?MODULE).

decode(X, Mod) ->
    decode(X, Mod, decode_init()).

decode(X, Mod, {more, Fun}) ->
    Fun(#state{x=X,mod=Mod}).

decode_init() ->
    {more, fun decode_start/1}.

decode_start(_S) ->
    %% @TODO
    exit(unimplemented).

decode_finish(#state{x=X,stack=Term}) ->
    {ok, Term, X}.

decode_pause(#state{x=X}=S, Cont, Resume) ->
    {more, fun(#state{x=X1,mod=Mod1}) ->
                   Resume(S#state{x= <<X/binary,X1/binary>>,mod=Mod1}, Cont)
           end}.

decode_error(Type, SubType, Value, S) ->
    {error, {Type, SubType, Value, S}}.


%%
%%---------------------------------------------------------------------
%%

%% @TODO encode implementation


%%
%%---------------------------------------------------------------------
%%

%% @TODO decode implementation


%%
%%---------------------------------------------------------------------
%%
atom_to_binary(X) ->
    list_to_binary(atom_to_list(X)).

binary_to_existing_atom(X) ->
    list_to_existing_atom(binary_to_list(X)).

push(X, [Top|Rest]) ->
    [[X|Top]|Rest].
