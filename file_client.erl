-module(file_client).

-compile(export_all).

-import(lists, [map/2, foldl/3, filter/2]).
-import(client, [rpc/2]).

defaultPort() -> 2000.

s(X) -> {'#S', X}.

-define(S(X), {'#S',X}).

    
test() ->
    {ok, Pid, Name} = client:start(host(), defaultPort()),
    {reply, _, start} = rpc(Pid, {startService, s("file_server"), []}),
    {reply, ok, active} = rpc(Pid, {logon, s("jimmy")}),
    {reply,  {files, Fs}, active} = rpc(Pid, ls),
    Files = map(fun(?S(I)) -> I end, Fs),
    io:format("Files=~p~n",[Files]),
    {reply, X, active} = rpc(Pid, {get, s("ubf.erl")}),
    io:format("got ~p bytes~n",[size(X)]),
    client:stop(Pid),
    io:format("test worked~n"),
    ok.

host() -> 
    case os:getenv("WHERE") of
	"HOME" -> "localhost";
	"WORK" ->  "p2p.sics.se"
    end.
