%%
%% For most purposes, these functions are not called by code outside of
%% this library: Erlang client &amp; Erlang server application code usually
%% have no need to use these functions.
%%
%% == Links ==
%%
%% <ul>
%% <li> http://hadoop.apache.org/avro/docs/current/spec.html </li>
%% <li> http://lucene.apache.org/java/2_4_0/fileformats.html#VInt </li>
%% <li> http://code.google.com/apis/protocolbuffers/docs/encoding.html#types </li>
%% </ul>
%%
%% == Avro Data Types ==
%% ```
%% typeval = prim/ complex
%% prim    = null/ boolean/ int/ long/ float/ double/ bytes/ string
%% complex = record/ enum/ array/ map/ union/ fixed
%%
%% null    = ""            ; zero-length
%% boolean = %x00/ %x01
%% int     = vint
%% long    = vint
%% float   = 4*OCTET       ; 32-bit IEEE 754 floating-point number
%% double  = 8*OCTET       ; 64-bit IEEE 754 floating-point number
%% bytes   = long *OCTET   ; bytes and that bytes of data
%% string  = long *OCTET   ; bytes and that bytes of utf-8 characters
%%
%% record  = *typeval      ; list of other type values decleared by the schema
%% enum    = int           ; zero-based position
%% array   = *ablock end
%% map     = *mblock end
%% union   = long typeval  ; zero-based position and its value
%% fixed   = *OCTET        ; length is declared in the schema
%%
%% end     = 0x00
%% ablock  = long *typeval ; number of array items and that many items
%% mblock  = long *keyval  ; number of key-val pairs and that many key-val pairs
%% keyval  = string typeval ; key is always string
%% vint    = OCTET *OCTET  ; variable length zig-zag encoding
%%       ; http://lucene.apache.org/java/2_4_0/fileformats.html#VInt </li>
%%       ; http://code.google.com/apis/protocolbuffers/docs/encoding.html#types </li>
%% '''
%% == Mapping: Avro -> Erlang Terms
%% ```
%% abf::null()     = null.
%% abf::boolean()  = true | false.
%% abf::int()      = integer().
%% abf::long()     = integer().
%% abf::float()    = float().
%% abf::double()   = float().
%% abf::bytes()    = binary().
%% abf::string()   = {'$S', [integer()]}.
%%
%% '''
%% == Mapping: UBF -> Erlang Terms ==
%% ```
%% todo
%% '''
%% == Mapping: UBF value -> Avro value ==
%% ```
%% todo
%% '''


-module(abf).

-include("ubf.hrl").

-export([encode/3]).
-export([decode_init/0, decode/3, decode/4]).


%%
%%---------------------------------------------------------------------
%%
-type state() :: term(). %% todo

-spec encode(Input::term(), module(), state()) -> {state(), iolist()}.

-type ok() :: {ok, Output::term(), Remainder::binary()}.
-type error() :: {error, Reason::term()}.
-type cont() :: {more, fun()}.

-spec decode_init() -> fun().
-spec decode(Input::binary(), module(), state()) -> {state(), ok()} | {state(), error()} | {state(), cont()}.
-spec decode(Input::binary(), module(), cont(), state()) -> {state(), ok()} | {state(), error()} | {state(), cont()}.


-record(state,{todo}).

%%
%%---------------------------------------------------------------------
%%
encode(_X, _Mod, State) when is_record(State, state) ->
    {State, <<"todo">>}.

%%
%%---------------------------------------------------------------------
%%
decode(_X, _Mod, State) ->
    {State, {ok, todo, <<"">>}}.

decode(_X, _Mod, _Cont, State) ->
    {State, {ok, todo, <<"">>}}.

decode_init() ->
    {more, []}. %% todo

