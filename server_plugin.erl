-module(server_plugin).

-import(server, [sendEvent/2, ask_manager/2]).
-import(lists, [map/2, member/2, foreach/2]).

-compile(export_all).

-compile({parse_transform,contract_parser}).
-add_contract("server_plugin").

 
s(Str) -> {'#S', Str}.
-define(S(Str), {'#S',Str}).

%% This is called when we start this manager
%% It returns a state

%% The server plugin only knows how to start it's sub-services

managerStart() ->
    {plugin_handler:start_manager(test_plugin),
     plugin_handler:start_manager(file_plugin),
     plugin_handler:start_manager(irc_plugin)}.

startService("test_server", State={P1, P2, P3}) ->
    {accept, test_plugin, P1, State};
startService("file_server", State={P1,P2,P3}) ->
    {accept, file_plugin, P2, State};
startService("irc_server", State={P1,P2,P3}) ->
    {accept, irc_plugin, P3, State};
startService(Str, State) ->
    {reject, noSuchService, State}.
	


