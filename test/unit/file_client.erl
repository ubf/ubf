-module(file_client).

-include("ubf.hrl").

-compile(export_all).

-import(lists, [map/2, foldl/3, filter/2]).
-import(ubf_client, [rpc/2]).

defaultPort() -> 2000.
fileNameToGet() -> "file_client.erl".

%% Prerequisites:
%%
%% 1. The current working directory must be ".", because test() will
%%    try to access the file fileNameToGet().
%%
%% 2. If the UBF server is not listening on "localhost", then your OS
%%    environment variable "WHERE" must be set to the hostname of the
%%    UBF server's DNS name (or, alternately, the UBF server's IP
%%    address).  See the host() function for details.
%%
%% 3. A UBF server must be listening to TCP port defaultPort() and
%%    have the "file_server" contract, which is implemented by the
%%    "file_plugin" module.
%%
%% Here is minimal recipe.
%%
%%   erl -pz ../../ebin
%%
%%   > ubf_server:start(undefined, [file_plugin], file_client:defaultPort(), []).
%%   > ok = file_client:test().

test() ->
    {ok, Pid, _Name} = ubf_client:connect(host(), defaultPort()),

    {reply, Info, _} = ubf_client:rpc(Pid,
                                      {startSession, ?S("file_server"), []}),
    io:format("Info = ~p~n",[Info]),

    {reply, {files, Fs}, start} = rpc(Pid, ls),
    Files = map(fun(?S(I)) -> I end, Fs),
    io:format("Files=~p~n",[Files]),

    FileName = fileNameToGet(),
    {reply, X, start} = rpc(Pid, {get, ?S(FileName)}),
    io:format("got ~p bytes for ~s~n",[size(X), FileName]),

    ubf_client:stop(Pid),
    io:format("test worked~n"),
    ok.

host() ->
    case os:getenv("WHERE") of
	false  -> "localhost";
        "HOME" -> "localhost";
        "WORK" ->  "p2p.sics.se"
    end.
