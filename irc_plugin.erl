-module(irc_plugin).

-export([manager_start/2, client_stop/3,
	 managerStart/0, handlerStartState/0, 
	 manager_rpc/2, handle_rpc/4]).

-import(server, [sendEvent/2, ask_manager/2]).
-import(lists, [delete/2, map/2, member/2, foreach/2]).

%% NOTE the following two lines

-compile({parse_transform,contract_parser}).
-add_contract("irc_plugin").

-define(S(X), {'#S',X}).
s(X) -> {'#S', X}.

managerStart() -> new_seed(), ets:new(irc, []).

handlerStartState() -> myHandlerState.
    
%% manager_start(Args, State) is called every time a session is started
%% Args comes from client:start(Host,Port,Service,Args)
%% Service in the rcp:start must match name()
%% manager_start(Args, State) -> {accept, Reply, State} | {reject, Why, State}
%% State is the manager state.

manager_start(_, ManagerState) ->
    {accept, yes, start, [], ManagerState}.

client_stop(Pid, Reason, Ets) ->
    io:format("Client stopped:~p ~p~n",[Pid, Reason]),
    {Nick, Groups} = facts(Ets, Pid),
    io:format("Pid has nick~p is in groups~p~n",[Nick, Groups]),
    ets:delete(Ets, {facts, Pid}),
    ets:delete(Ets, {pid, Nick}),
    foreach(fun(G) ->
		    Pids = pids(Ets, G),
		    Pids1 = delete(Pid, Pids),
		    ets:insert(Ets, {{group,G}, Pids1}),
		    broadcast_to_group(Pids1, {leaves, s(Nick), s(G)})
	    end, Groups),
    Ets.

%% The manager state
%% Is what ??
%% We know the Pid of the Client
%% The         Nick of the client on the Pid
%% The set of groups that the client is joined to

%% This is all in one ets table
%%     {group, Name} => [Pids]  (all the Pids in a group)
%%                      {group, "erlang"} => [Pid1, Pid2, ...]
%%     {facts, Pid} => {Nick, [Group]}
%%                      The nick and the List of joined groups
%%                      for Pid
%%     {pid, Nick} => Pid

%% When somebody joins or leaves a group broadcast that
%% they have joined or leaved the group
%% When somebody dies remove their nick
%% and remove them from all groups

pids(Ets, Name) ->
    case ets:lookup(Ets, {group, Name}) of
	[{_,L}] -> L;
	[] -> []
    end.

facts(Ets, Pid) ->
    case ets:lookup(Ets, {facts, Pid}) of
	[{_,L}] -> L;
	[] -> []
    end.
	    
manager_rpc({join, Pid, Group}, Ets) ->
    %% Nick (Pid) joins the group G
    {Nick, Gs} = facts(Ets, Pid),
    case member(Group, Gs) of
	true  -> {ok, Ets};
	false ->
	    ets:insert(Ets, {{facts, Pid}, {Nick,[Group|Gs]}}),
	    Pids = [Pid|pids(Ets, Group)],
	    ets:insert(Ets, {{group,Group}, Pids}),
	    broadcast_to_group(Pids, {joins, s(Nick), s(Group)}),
	    {ok, Ets}
    end;
manager_rpc({leave, Pid, Group}, Ets) ->
    {Nick, Gs} = facts(Ets, Pid),
    case member(Group, Gs) of
	false -> {ok, Ets};
	true ->
	    ets:insert(Ets, {{facts, Pid}, {Nick,delete(Group, Gs)}}),
	    Pids = delete(Pid, pids(Ets, Group)),
	    ets:insert(Ets, {{group,Group}, Pids}),
	    broadcast_to_group(Pids, {leaves, s(Nick), s(Group)}),
	    {ok, Ets}
    end;
manager_rpc({msg, Pid, Group, Msg}, Ets) ->
    {Nick, Gs} = facts(Ets, Pid),
    case member(Group, Gs) of
	false -> {notJoined, Ets};
	true ->
	    broadcast_to_group(pids(Ets, Group), 
			       {msg, s(Nick), s(Group), s(Msg)}),
	    {ok, Ets}
    end;
manager_rpc(groups, Ets) ->
    M = ets:match(Ets, {{group,'$1'},'_'}),
    io:format("Here Groups=~p~n",[M]),
    Strs = map(fun([I]) -> s(I) end, M),
    {Strs, Ets};
manager_rpc(P={logon, Pid}, Ets) ->
    Nick = random_nick(6),
    case ets:lookup(Ets, {pid, Nick}) of
	[] ->
	    ets:insert(Ets, {{pid, Nick}, Pid}),
	    ets:insert(Ets, {{facts,Pid}, {Nick, []}}),
	    {Nick, Ets};
	_ ->
	    manager_rpc(P, Ets)
    end;
manager_rpc({change_nick,Old,New,Pid}, Ets) ->
    case ets:lookup(Ets, {pid, New}) of
	[] ->
	    ets:insert(Ets, {{pid, New}, Pid}),
	    ets:delete(Ets, {pid,Old}),
	    {_, Groups} = facts(Ets, Pid),
	    ets:insert(Ets, {{facts,Pid},{New, Groups}}),
	    %% Now tell all groups about the name change
	    foreach(fun(G) ->
			    Pids = pids(Ets, G),
			    broadcast_to_group(Pids, {changesName, 
						      s(Old), s(New), 
						      s(G)})
		    end, Groups),
	    {ok, Ets};
	_ ->
	    {error, Ets}
    end.

broadcast_to_group(L, Msg) ->
    foreach(fun(Pid) -> sendEvent(Pid, Msg) end, L).
    
handle_rpc(start, logon, State, Manager) ->
    R = ask_manager(Manager, {logon, self()}),
    {{ok, s(R)}, active, R};
handle_rpc(active, {join, ?S(Group)}, Nick, Manager) ->
    ask_manager(Manager, {join, self(), Group}),
    {ok, active, Nick};
handle_rpc(active, {leave, ?S(Group)}, Nick, Manager) ->
    ask_manager(Manager, {leave, self(), Group}),
    {ok, active, Nick};
handle_rpc(active, {msg, ?S(Group), ?S(Msg)}, Nick, Manager) ->
    ask_manager(Manager, {msg, self(), Group, Msg}),
    {ok, active, Nick};
handle_rpc(active, {nick, ?S(New)}, Nick, Manager) ->
    case ask_manager(Manager, {change_nick,Nick,New,self()}) of
	ok ->
	    {nickChanged, active, New};
	error ->
	    {nickInUse, active, Nick}
    end;
handle_rpc(active, groups, Nick, Manager) ->
    Groups = ask_manager(Manager, groups),
    {Groups, active, Nick}.

random_nick(0) ->
    [];
random_nick(N) ->
    [$a + random:uniform(26) - 1|random_nick(N-1)].

new_seed() ->
    {_,_,X} = erlang:now(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}).


	    
				     





