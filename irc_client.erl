-module(irc_client).

-compile(export_all).
-import(client, [rpc/2]).


-define(S(X), {'#S',X}).
s(X) -> {'#S', X}.

batch([Name]) ->
    start(atom_to_list(Name)),
    erlang:halt().

start(Nick) ->
    {ok, Pid, Name} = client:start("localhost", 2000),
    {reply, {ok,yes}, start} = rpc(Pid, {startService, s("irc"), []}),
    client:install_handler(Pid, fun print_msg/1),
    {reply, _, _} = rpc(Pid, logon),
    case rpc(Pid, {nick, s(Nick)}) of
	{reply, nickInUse, _} ->
	    client:stop(Pid);
	{reply, nickChanged, active} ->
	    {reply,_,_}   = rpc(Pid, {join, s("erlang")}),
	    loop(Pid, "erlang", ["erlang"], Nick)
    end,
    io:format("client stops~n").

loop(Pid, Group, Gs, Nick) ->
    io:format("Status: Nick=~s Group=~s Joined groups=~p~n",[Nick, Group, Gs]),
    case io:get_line('> ') of
	"\\" ++ T ->
	    case string:tokens(T, "\s\n\t\r") of
		["group",G] ->
		    loop(Pid, G, Gs, Nick);
		["ls"] ->
		    {reply, G, _} = rpc(Pid, groups),
		    io:format("Groups=~p~n",[G]),
		    loop(Pid, Group, Gs, Nick);
		["join", G] ->
		    {reply, _, _} = rpc(Pid, {join, s(G)}),
		    loop(Pid, Group, [G|Gs], Nick);
		["leave", G] ->
		    rpc(Pid, {leave, s(G)}),		
		    Gs1 = lists:delete(G, Gs),
		    loop(Pid, Group, [G|Gs], Nick);
		["nick", N] ->
		    case rpc(Pid, {nick, s(N)}) of
			{reply, nickInUse, _} ->
			    loop(Pid, Group, Gs, Nick);
			{reply, nickChanged, active} ->
			    loop(Pid, "erlang", ["erlang"], N)
		    end;
		["quit"] ->
		    client:stop(Pid),
		    true;
	    _ ->
		    %% io:format("OOPs:~p~n",[X]),
		    banner(),
		    loop(Pid, Group, Gs, Nick)
		
	     end;
	Msg ->
	    case rpc(Pid, {msg, s(Group), s(Msg)}) of
		{reply, R,_} ->
		    loop(Pid, Group, Gs, Nick);
		{error, stop, stop} ->
		    io:format("The server died :-(~n")
	    end
    end.

banner() ->	    
    io:format("\\group G -- change to group G\n"
              "\\ls      -- list groups\n"
              "\\nick N  -- change nick to N\n"
              "\\join G  -- join G\n"
              "\\leave G -- leave G\n"
	      "\\quit    -- quit\n"
              "Msg      -- send msg with current N to G\n").

print_msg(X) ->
    case ubf:deabstract(X) of
	{joins, Who, Group} ->
	    io:format("~s joins the group ~s~n",[Who, Group]);
	{leaves, Who, Group} ->
	    io:format("~s leaves the group ~s~n",[Who, Group]);
	{msg, Who, Group, Msg} ->
	    io:format("Msg from ~s to ~s => ~s~n",[Who, Group,Msg]);
	{changesName, Old, New, Group} ->
	    io:format("~s is now called ~s in group ~s~n",
		      [Old, New, Group]);
	Other ->
	    io:format("==> ~p~n",[Other])
    end,
    fun print_msg/1.



