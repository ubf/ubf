-module(file_client).

-compile(export_all).

-import(lists, [map/2, foldl/3, filter/2]).
-import(client, [rpc/2]).

defaultPort() -> 2000.

s(X) -> {'#S', X}.

-define(S(X), {'#S',X}).

    
test() ->
    {ok, Pid, Name} = client:connect(host(), defaultPort()),
    {reply, Info, _} = client:rpc(Pid, 
				      {startService, s("file_server"), []}),
    io:format("Info=~p~n",[Info]),
    {reply,  {files, Fs}, start} = rpc(Pid, ls),
    Files = map(fun(?S(I)) -> I end, Fs),
    io:format("Files=~p~n",[Files]),
    {reply, X, start} = rpc(Pid, {get, s("ubf.erl")}),
    io:format("got ~p bytes~n",[size(X)]),
    client:stop(Pid),
    io:format("test worked~n"),
    ok.

host() -> 
    case os:getenv("WHERE") of
	"HOME" -> "localhost";
	"WORK" ->  "p2p.sics.se"
    end.
