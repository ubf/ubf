%% @doc Functions for JSON&lt;->Erlang data conversion.
%%
%% For most purposes, these functions are not called by code outside of
%% this library: Erlang client &amp; Erlang server application code usually
%% have no need to use these functions.
%%
%% == Links ==
%%
%% <ul>
%% <li> http://www.erlang-projects.org/Public/news/ejson/view </li>
%% <li> http://www.erlang.org/eeps/eep-0018.html </li>
%% <li> http://www.erlang.org/ml-archive/erlang-questions/200511/msg00193.html </li>
%% <li> http://www.ietf.org/rfc/rfc4627.txt </li>
%% <li> http://www.json.org/ </li>
%% <li> http://www.lshift.net/blog/2007/02/17/json-and-json-rpc-for-erlang </li>
%% <li> http://www.json.com/json-schema-proposal/ </li>
%% </ul>
%%
%% == JSON Basic Data Types ==
%% ```
%% object
%%           {}
%%           { members }
%% members
%%           pair
%%           pair, members
%% pair
%%           string : value
%% array
%%          []
%%          [ elements ]
%% elements
%%          value
%%          value, elements
%% value
%%          string
%%          number
%%          object
%%          true (atom)
%%          false (atom)
%%          null (atom)
%% '''
%%
%% == Mapping: JSON -> Erlang Terms, using erlang-rfc46267 ==
%% ```
%% json::object() = {obj, [json::pair()]}
%%
%% json::pair() = {string(), json::value()}
%%      string() = [byte()]
%%      byte() = integer()
%%
%% json::array() = [json::value()]
%%
%% json::value() = json::object() | json::array() | json::number() | json::string() | json::true() | json::false() | json::null()
%%
%% json::number() = integer() | float()
%%
%% json::string() = binary()
%%
%% json::true() = true
%% json::false() = false
%% json::null() = null
%% '''
%%
%% == Mapping: UBF -> Erlang Terms ==
%% ```
%% ubf::tuple() = tuple()
%%
%% ubf::list() = list()
%%
%% ubf::number = integer() | float()
%%
%% ubf::string() = {'$S', [integer()]}
%%
%% ubf::proplist() = {'$P', [{term(), term()}]}
%%
%% ubf::binary() = binary()
%%
%% ubf::true() = true
%% ubf::false() = false
%% ubf::undefined() = undefined
%%
%% ubf::atom() = atom()
%%
%% ubf::record() = record()
%% '''
%%
%% == Mapping: UBF value -> JSON value ==
%% ```
%% ubf::tuple() = {obj, [{"$T", ubf::list()}]}
%%
%% ubf::list() = [value()]
%%
%% ubf::number() = integer() | float()
%%
%% ubf::string() = {obj, [{"$S", binary()}]}
%%
%% ubf::proplist() = {obj, [{binary(), value()}]}
%%
%% ubf::binary() = binary()
%%
%% ubf::true() = true
%% ubf::false() = false
%% ubf::undefined() = null
%%
%% ubf::atom() = {obj, [{"$A", atomname()}]}
%%      atomname() = binary()  % a.k.a. list_to_binary(atom_to_list()) for the actual atom
%%
%% ubf::record() = {obj, [{"$R", recordname()}] ++ [recordpair()]}
%%      recordname() = binary()  % a.k.a. list_to_binary(atom_to_list()) for the record's name
%%      recordpair() = {recordkey(), value()}
%%      recordkey() = binary()  % a.k.a. list_to_binary(atom_to_list()) for the record key's name
%%
%% value() = ubf::tuple() | ubf::list() | ubf::number() | ubf::string() | ubf::binary() | ubf::true() | ubf::false() | ubf::undefined() | ubf::atom() | ubf::record()
%% '''

-module(jsf).

-include("ubf.hrl").

-export([encode_print/2, encode/2, do_encode/2]).
-export([decode_print/2, decode/2, do_decode/2]).

-export([atom_to_binary/1]).
-export([binary_to_existing_atom/1]).


%% Dummy hack/kludge.
-export([contract_records/0]).

contract_records() ->
    [].


%%
%%---------------------------------------------------------------------
%%

encode_print(X, UBFMod) ->
    io:format("~s~n", [encode(X, UBFMod)]).

encode(X, UBFMod) ->
    rfc4627:encode(do_encode(X, UBFMod)).

do_encode(X, _UBFMod) when is_binary(X); is_integer(X); is_float(X) ->
    X;
do_encode(X, _UBFMod) when is_atom(X) ->
    encode_atom(X);
do_encode(X, UBFMod) when is_list(X) ->
    encode_list(X, UBFMod);
do_encode(?S(X), _UBFMod) ->
    encode_string(X);
do_encode(?P(X), UBFMod) ->
    encode_proplist(X, UBFMod);
do_encode(X, UBFMod) when is_tuple(X) ->
    encode_tuple(X, UBFMod).

encode_atom(true) ->
    true;
encode_atom(false) ->
    false;
encode_atom(undefined) ->
    null;
encode_atom(X) ->
    {obj, [{"$A", atom_to_binary(X)}]}.

encode_list(L, UBFMod) ->
    encode_list(L, [], UBFMod).

encode_list([], Acc, _UBFMod) ->
    lists:reverse(Acc);
encode_list([H|T], Acc, UBFMod) ->
    NewAcc = [do_encode(H, UBFMod)|Acc],
    encode_list(T, NewAcc, UBFMod).

encode_string(X) when is_list(X) ->
    {obj, [{"$S", list_to_binary(X)}]}.

encode_proplist(X, UBFMod) when is_list(X) ->
    {obj, [ {K, do_encode(V, UBFMod)} || {K, V} <- X ]}.

encode_tuple({}, _UBFMod) ->
    {obj, [{"$T", []}]};
encode_tuple(X, UBFMod) when not is_atom(element(1, X)) ->
    {obj, [{"$T", encode_tuple(1, X, [], UBFMod)}]};
encode_tuple(X, UBFMod) ->
    RecName = element(1, X),
    Y = {RecName, tuple_size(X)-1},
    case lists:member(Y, UBFMod:contract_records()) of
        false ->
            {obj, [{"$T", encode_tuple(1, X, [], UBFMod)}]};
        true ->
            %% @todo optimize this code
            Keys = list_to_tuple(UBFMod:contract_record(Y)),
            {obj, [{"$R", atom_to_binary(RecName)}|encode_record(2, X, Keys, [], UBFMod)]}
    end.

encode_tuple(N, X, Acc, _UBFMod) when is_integer(N), N > tuple_size(X) ->
    lists:reverse(Acc);
encode_tuple(N, X, Acc, UBFMod) ->
    NewAcc = [do_encode(element(N, X), UBFMod)|Acc],
    encode_tuple(N+1, X, NewAcc, UBFMod).

encode_record(N, X, _Keys, Acc, _UBFMod) when is_integer(N), N > tuple_size(X) ->
    Acc;
encode_record(N, X, Keys, Acc, UBFMod) ->
    NewAcc = [{atom_to_binary(element(N-1, Keys)), do_encode(element(N, X), UBFMod)}|Acc],
    encode_record(N+1, X, Keys, NewAcc, UBFMod).


%%
%%---------------------------------------------------------------------
%%
decode_print(X, UBFMod) ->
    io:format("~s~n", [decode(X, UBFMod)]).

decode(X, UBFMod) ->
    case rfc4627:decode(X) of
        {ok, JSON, Remainder} ->
            {ok, do_decode(JSON, UBFMod), Remainder};
        Else ->
            Else
    end.

do_decode(X, _UBFMod) when is_binary(X); is_integer(X); is_float(X) ->
    X;
do_decode(X, _UBFMod) when is_atom(X) ->
    decode_atom(X);
do_decode(X, UBFMod) when is_list(X) ->
    decode_list(X, UBFMod);
do_decode({obj, [{"$A", X}]}, _UBFMod) ->
    decode_atom(X);
do_decode({obj, [{"$S", X}]}, _UBFMod) ->
    decode_string(X);
do_decode({obj, [{"$T", X}]}, UBFMod) ->
    decode_tuple(X, [], UBFMod);
do_decode({obj, X}, UBFMod) ->
    case lists:keytake("$R", 1, X) of
        {value, {"$R", RecName}, Y} ->
            decode_record(RecName, Y, UBFMod);
        false ->
            decode_proplist(X, UBFMod)
    end.

decode_atom(true) ->
    true;
decode_atom(false) ->
    false;
decode_atom(null) ->
    undefined;
decode_atom(X) when is_binary(X) ->
    binary_to_existing_atom(X).

decode_list(L, UBFMod) ->
    decode_list(L, [], UBFMod).

decode_list([], Acc, _UBFMod) ->
    lists:reverse(Acc);
decode_list([H|T], Acc, UBFMod) ->
    NewAcc = [do_decode(H, UBFMod)|Acc],
    decode_list(T, NewAcc, UBFMod).

decode_string(X) when is_binary(X) ->
    ?S(binary_to_list(X)).

decode_proplist(X, UBFMod) when is_list(X) ->
    ?P([ {list_to_binary(K), do_decode(V, UBFMod)} || {K, V} <- X ]).

decode_tuple([], Acc, _UBFMod) ->
    list_to_tuple(lists:reverse(Acc));
decode_tuple([H|T], Acc, UBFMod) ->
    NewAcc = [do_decode(H, UBFMod)|Acc],
    decode_tuple(T, NewAcc, UBFMod).

decode_record(RecNameStr, X, UBFMod) ->
    RecName = binary_to_existing_atom(RecNameStr),
    Y = {RecName, length(X)},
    Keys = UBFMod:contract_record(Y),
    decode_record(RecName, Keys, X, [], UBFMod).

decode_record(RecName, [], [], Acc, _UBFMod) ->
    list_to_tuple([RecName|lists:reverse(Acc)]);
decode_record(RecName, [H|T], X, Acc, UBFMod) ->
    K = atom_to_list(H),
    case lists:keytake(K, 1, X) of
        {value, {K, V}, NewX} ->
            NewAcc = [do_decode(V, UBFMod)|Acc],
            decode_record(RecName, T, NewX, NewAcc, UBFMod);
        false ->
            exit({badrecord, RecName})
    end.


%%
%%---------------------------------------------------------------------
%%

atom_to_binary(X) ->
    list_to_binary(atom_to_list(X)).

binary_to_existing_atom(X) ->
    list_to_existing_atom(binary_to_list(X)).
