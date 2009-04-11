-module(jsf).

-include("ubf.hrl").

-compile(export_all).

-export([rpc_v11_req_encode_print/2, rpc_v11_req_encode_print/3]).
-export([rpc_v11_req_encode/2, rpc_v11_req_encode/3]).

-export([rpc_v11_req_decode_print/1, rpc_v11_req_decode_print/2, rpc_v11_req_decode_print/3]).
-export([rpc_v11_req_decode/1, rpc_v11_req_decode/2, rpc_v11_req_decode/3]).

-export([rpc_v11_res_encode_print/2, rpc_v11_res_encode_print/3]).
-export([rpc_v11_res_encode/2, rpc_v11_res_encode/3]).

-export([rpc_v11_res_decode_print/1, rpc_v11_res_decode_print/2]).
-export([rpc_v11_res_decode/1, rpc_v11_res_decode/2]).

-export([encode_print/1, encode_print/2]).
-export([encode/1, encode/2]).

-export([decode_print/1, decode_print/2]).
-export([decode/1, decode/2]).

%%
%% Links:
%%   http://json-rpc.org/wd/JSON-RPC-1-1-WD-20060807.html
%%   http://json-rpc.org/wiki/specification
%%   http://www.erlang-projects.org/Public/news/ejson/view
%%   http://www.erlang.org/eeps/eep-0018.html
%%   http://www.erlang.org/ml-archive/erlang-questions/200511/msg00193.html
%%   http://www.ietf.org/rfc/rfc4627.txt
%%   http://www.json.org/
%%   http://www.lshift.net/blog/2007/02/17/json-and-json-rpc-for-erlang
%%
%% Other Links:
%%   http://www.json.com/json-schema-proposal/
%%

%%
%% json
%%

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


%%
%% json-rpc
%%

%%
%% @todo - pending reviewing against JSON-RPC 1.1 Specification
%% Working Draft 7 August 2006

%% The following list (non-comprehensive) summarizes the general changes that have been applied to version 1.0:

%%     * JSON-RPC 1.1 is bound to HTTP only. Use over other protocols are not considered normative according to verson 1.1.
%%     * Renamed request to Procedure Call (capitalization significant).
%%     * Renamed response to Procedure Return (capitalization significant).
%%     * Removed notifications.
%%     * Removed the requirement to specify id for regular calls.
%%     * Formalize Content-Type for JSON-RPC.
%%     * Add version member to Procedure Call and Procedure Return objects.
%%     * Formalization of the JSON type system, i.e. Boolean, String, Number, Array, Object and the Null value (capitalization significant).
%%     * Added call encoding for HTTP GET.
%%     * Added named and positional arguments.
%%     * Added Error object and formalization of error codes.
%%     * Added introspection (service description) and required system procedures.


%% 1.1 Request (method invocation)
%%
%% A remote method is invoked by sending a request to a remote
%% service. The request is a single object serialized using JSON.
%%
%% It has three properties:
%%
%% method - A String containing the name of the method to be invoked.
%%
%% params - An Array of objects to pass as arguments to the method.
%%
%% id - The request id. This can be of any type. It is used to match the
%% response with the request that it is replying to.
%%
%% 1.2 Response
%%
%% When the method invocation completes, the service must reply with a
%% response. The response is a single object serialized using JSON.
%%
%% It has three properties:
%%
%% result - The Object that was returned by the invoked method. This must
%% be null in case there was an error invoking the method.
%%
%% error - An Error object if there was an error invoking the method. It
%% must be null if there was no error.
%%
%% id - This must be the same id as the request it is responding to.
%%
%% 1.3 Notification
%%
%% A notification is a special request which does not have a
%% response. The notification is a single object serialized using JSON.
%%
%% It has the same properties as the request object with one exception.
%% id - Must be null.


%%
%% erlang-rfc46267
%%

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


%%
%% ubf
%%

%% ubf::tuple() = tuple()
%%
%% ubf::list() = list()
%%
%% ubf::number = integer() | float()
%%
%% ubf::string() = {'$S', [integer()]}
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


%%
%% encode(ubf::value()) -> json::value()
%%

%% ubf::tuple() = {obj, [{"$T", ubf::list()}]}
%%
%% ubf::list() = [value()]
%%
%% ubf::number() = integer() | float()
%%
%% ubf::string() = {obj, [{"$S", binary()}]}
%%
%% ubf::binary() = binary()
%%
%% ubf::true() = true
%% ubf::false() = false
%% ubf::undefined() = null
%%
%% ubf::atom() = {obj, [{"$A", atomname()}
%%      atomname() = binary()  % a.k.a. list_to_binary(atom_to_list()) for the actual atom
%%
%% ubf::record() = {obj, [{"$R", recordname()}] ++ [recordpair()]}
%%      recordname = binary()  % a.k.a. list_to_binary(atom_to_list()) for the record's name
%%      recordpair() = {recordkey(), value()}
%%      recordkey() = binary()  % a.k.a. list_to_binary(atom_to_list()) for the record key's name
%%
%% value() = ubf::tuple() | ubf::list() | ubf::number() | ubf::string() | ubf::binary() | ubf::true() | ubf::false() | ubf::undefined() | ubf::atom() | ubf::record()


%%
%%---------------------------------------------------------------------
%%

rpc_v11_req_encode_print(X, Id) ->
    rpc_v11_req_encode_print(X, Id, []).

rpc_v11_req_encode_print(X, Id, UBFMod) ->
    io:format("~s~n", [rpc_v11_req_encode(X, Id, UBFMod)]).

rpc_v11_req_encode(X, Id) ->
    rpc_v11_req_encode(X, Id, []).

rpc_v11_req_encode(Method, Id, _UBFMod) when is_atom(Method) ->
    {undefined, {obj, [{"method", atom_to_binary(Method)}, {"params", []}, {"id", Id}, {"version", <<"1.1">>}]}};

rpc_v11_req_encode(X, Id, UBFMod) when is_tuple(X), size(X) > 1, is_atom(element(1, X)) ->
    [Method|[AuthInfo|Params]] = tuple_to_list(X),
    EncodedParams = do_encode(Params,UBFMod),
    {AuthInfo, {obj, [{"method", atom_to_binary(Method)}, {"params", EncodedParams}, {"id", Id}, {"version", <<"1.1">>}]}}.


%%
%%---------------------------------------------------------------------
%%

rpc_v11_req_decode_print(X) ->
    rpc_v11_req_decode_print(undefined, X).

rpc_v11_req_decode_print(AuthInfo, X) ->
    rpc_v11_req_decode_print(AuthInfo, X, []).

rpc_v11_req_decode_print(AuthInfo, X, UBFMod) ->
    io:format("~s~n", [rpc_v11_req_decode(AuthInfo, X, UBFMod)]).

rpc_v11_req_decode(X) ->
    rpc_v11_req_decode(undefined, X).

rpc_v11_req_decode(AuthInfo, X) ->
    rpc_v11_req_decode(AuthInfo, X, []).

rpc_v11_req_decode(AuthInfo, X, UBFMod) ->
    case rfc4627:decode(X) of
        {ok,{obj,[{"method",MethodBin},
                  {"params",JsonParams},
                  {"id",Id},
                  {"version",<<"1.1">>}
                 ]},
         []} ->
            Method = binary_to_existing_atom(MethodBin),
            Params = do_decode(JsonParams,UBFMod),
            if Params =:= [] ->
                    {ok, Method, Id};
               true ->
                    if AuthInfo =:= undefined ->
                            {ok, list_to_tuple([Method|Params]), Id};
                       true ->
                            {ok, list_to_tuple([Method|[AuthInfo|Params]]), Id}
                    end
            end;
        Other -> { error, Other}
    end.


%%
%%---------------------------------------------------------------------
%%

rpc_v11_res_encode_print(X, Id) ->
    rpc_v11_res_encode_print(X, Id, []).

rpc_v11_res_encode_print(X, Id, UBFMod) ->
    io:format("~s~n", [rpc_v11_res_encode(X, Id, UBFMod)]).

rpc_v11_res_encode(X, Id) ->
    rpc_v11_res_encode(X, Id, []).

rpc_v11_res_encode(X, Id, UBFMod)  ->
    Rsp = do_encode(X,UBFMod),
    Y = {obj, [{<<"result">>, Rsp}, {<<"error">>, null}, {<<"id">>, Id}, {<<"version">>, <<"1.1">>}]},
    rfc4627:encode(Y).


%%
%%---------------------------------------------------------------------
%%

rpc_v11_res_decode_print(X) ->
    rpc_v11_res_decode_print(X, []).

rpc_v11_res_decode_print(X, UBFMod) ->
    io:format("~s~n", [rpc_v11_res_decode(X, UBFMod)]).

rpc_v11_res_decode(X) ->
    rpc_v11_res_decode(X, []).

rpc_v11_res_decode(X, _UBFMod) ->
    case rfc4627:decode(X) of
        {ok, {obj, Props}, []} ->
            {value, {"result", Result}, Props1} = keytake("result", 1, Props),
            {value, {"error", Error}, Props2} = keytake("error", 1, Props1),
            {value, {"id", Id}, [_Ver]} = keytake("id", 1, Props2),
            {ok, do_decode(Result,[]), Error, Id}
    end.


%%
%%---------------------------------------------------------------------
%%

encode_print(X) ->
    encode_print(X, []).

encode_print(X, UBFMod) ->
    io:format("~s~n", [encode(X, UBFMod)]).

encode(X) ->
    encode(X, []).

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

encode_tuple({}, _UBFMod) ->
    {obj, [{"$T", []}]};
encode_tuple(X, UBFMod) when not is_atom(element(1, X)) ->
    {obj, [{"$T", encode_tuple(1, X, [], UBFMod)}]};
encode_tuple(X, UBFMod) ->
    RecName = element(1, X),
    Y = {RecName, size(X)},
    case lists:keysearch(Y, 1, UBFMod) of
        false ->
            {obj, [{"$T", encode_tuple(1, X, [], UBFMod)}]};
        {value, {Y, Keys}} ->
            {obj, [{"$R", atom_to_binary(RecName)}|encode_record(2, X, Keys, [], UBFMod)]}
    end.

encode_tuple(N, X, Acc, _UBFMod) when is_integer(N), N > size(X) ->
    lists:reverse(Acc);
encode_tuple(N, X, Acc, UBFMod) ->
    NewAcc = [do_encode(element(N, X), UBFMod)|Acc],
    encode_tuple(N+1, X, NewAcc, UBFMod).

encode_record(N, X, _Keys, Acc, _UBFMod) when is_integer(N), N > size(X) ->
    Acc;
encode_record(N, X, Keys, Acc, UBFMod) ->
    NewAcc = [{atom_to_binary(element(N-1, Keys)), do_encode(element(N, X), UBFMod)}|Acc],
    encode_record(N+1, X, Keys, NewAcc, UBFMod).


%%
%%---------------------------------------------------------------------
%%

decode_print(X) ->
    decode_print(X, []).

decode_print(X, UBFMod) ->
    io:format("~s~n", [decode(X, UBFMod)]).

decode(X) ->
    decode(X, []).

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
    case keytake("$R", 1, X) of
        {value, {"$R", RecName}, Y} ->
            decode_record(RecName, Y, UBFMod);
        _Other -> do_decode(X,UBFMod)
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

decode_tuple([], Acc, _UBFMod) ->
    list_to_tuple(lists:reverse(Acc));
decode_tuple([H|T], Acc, UBFMod) ->
    NewAcc = [do_decode(H, UBFMod)|Acc],
    decode_tuple(T, NewAcc, UBFMod).

decode_record(RecName, X, UBFMod) ->
    Y = {RecName, length(X)},
    case lists:keysearch(Y, 1, UBFMod:get_records()) of
        {value, Z} ->
            Keys = UBFMod:get_record(Z),
            decode_record(RecName, Keys, X, [], UBFMod)
    end.

decode_record(RecName, [], [], Acc, _UBFMod) ->
    list_to_tuple([binary_to_existing_atom(RecName)|lists:reverse(Acc)]);
decode_record(RecName, [H|T], X, Acc, UBFMod) ->
    K = atom_to_binary(H),
    case keytake(K, 1, X) of
        {value, {K, V}, NewX} ->
            NewAcc = [do_decode(V, UBFMod)|Acc],
            decode_record(RecName, T, NewX, NewAcc, UBFMod)
    end.


%%
%%---------------------------------------------------------------------
%%

%% very useful but not in R11B-5 :( insert our own version for now

keytake(Key, N, L) when is_integer(N), N > 0 ->
    keytake(Key, N, L, []).

keytake(Key, N, [H|T], L) when element(N, H) == Key ->
    {value, H, lists:reverse(L, T)};
keytake(Key, N, [H|T], L) ->
    keytake(Key, N, T, [H|L]);
keytake(_K, _N, [], _L) -> false.

atom_to_binary(X) ->
    list_to_binary(atom_to_list(X)).

binary_to_existing_atom(X) ->
    list_to_existing_atom(binary_to_list(X)).
