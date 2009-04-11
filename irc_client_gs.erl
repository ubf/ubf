-module(irc_client_gs).

-compile(export_all).

-import(client, [rpc/2]).	  

-define(S(X), {'#S',X}).
s(X) -> {'#S', X}.

start() ->
    spawn(fun() -> init1() end).
	
init1() ->
    {ok, Pid, Name} = client:start("localhost", 2000),
    {reply, {ok,yes}, start} = rpc(Pid, {startService, s("irc"), []}),
    Self = self(),
    client:install_handler(Pid, fun(M) ->
					send_self(M, Self)
				end),
    case rpc(Pid, logon) of
	{reply, {ok,?S(Nick)}, active} ->
	    init(Pid, Nick);
	_ ->
	    exit(connect)
    end.

init(Pid, Nick) ->
    S=gs:start(),
    Width=250,Height=170,
    W= gs:window(S,[{title,"IRC client"},
		 {width,Width},{height,Height},{map,true}]),
    L1 = gs:label(W, [{x,10},{y,10},{label,{text,"Nick:" ++ Nick}}]),
    E1=gs:entry(W, [{x,10},{y,40},{width, 120}]),
    gs:button(W,[{x,130},{y,40},{data, join},{label,{text,"Join Group"}}]),
    E2=gs:entry(W, [{x,10},{y,70},{width, 120}]),
    gs:button(W,[{x,130},{y,70}, {data,nick},{label,{text,"Change Nick"}}]),
    gs:button(W,[{x,10},{y,110}, {data,quit}, {label,{text,"Quit"}}]),
    loop(S, dict:new(), Pid, L1, E1,E2).

loop(S, Dict, Pid, L1, E1, E2) ->
    receive
	{gs,_,click,nick,_} ->
	    Nick = gs:read(E2, text),
	    io:format("Change nick to:~s~n",[Nick]), 
	    case rpc(Pid, {nick, s(Nick)}) of
		{reply, nickInUse, _} ->
		    gs:config(E2, {text, "** bad nick **"}),
		    loop(S, Dict, Pid, L1, E1, E2);
		{reply, nickChanged, active} ->
		    io:format("nick was changed~n"),
		    gs:config(L1, {label, {text, "Nick: " ++ Nick}}),
		    loop(S, Dict, Pid, L1, E1, E2)
	    end,
	    loop(S, Dict, Pid, L1, E1, E2);
	{gs,_,click,join,_} ->
	    Group = gs:read(E1, text),
	    io:format("Join:~s~n",[Group]),
	    case Group of
		"" ->
		    loop(S, Dict, Pid, L1, E1, E2);
		_ ->
		    case dict:find(Group, Dict) of
			{ok, W} ->
			    loop(S, Dict, Pid, L1, E1, E2);
			error ->
			    W = new_group(S, Group),
			    rpc(Pid, {join, s(Group)}),
			    loop(S, dict:store(Group, W, Dict), Pid, 
				 L1, E1, E2)
		    end
	    end;
	{gs,_,click,quit,_} ->
	    exit(1); 
	{gs,Obj,keypress,G,['Return'|_]} ->
	    Str = gs:read(Obj, text),
	    io:format("Send: ~s to ~s~n",[Str, G]),
	    rpc(Pid, {msg, s(G), s(Str ++ "\n")}), 
	    loop(S, Dict, Pid, L1, E1, E2);
	{event, {msg, From, Group, Msg}} ->
	    case dict:find(Group, Dict) of
		{ok, {W, Txt}} ->
		    gs:config(Txt, {insert, {'end', From ++ " > " ++ Msg}});
		error ->
		    io:format("Msg:~s ~s ~s~n",[From, Group, Msg])
	    end,
	    loop(S, Dict, Pid, L1, E1, E2);
	X -> 
	    io:format("man: got other: ~w~n",[X]),
	    loop(S, Dict, Pid, L1, E1, E2)
    end.

new_group(S, Name) ->
    Width=450,Height=350,
    W  = gs:window(S,[{title,"Name"},
		    {width,Width},{height,Height},{map,true}]),
    L1 = gs:label(W, [{x,10},{y,10},{label,{text,"Group:" ++ Name}}]),
    T1 = gs:editor(W,  [{x,10},{y,40}, 
			{width,Width-20},
			{height,Height-120},
			{vscroll, right}]), 
    E1 = gs:entry(W, [{x,10},{y,Height-70},{width, Width-20},
		      {data, Name},
		      {keypress,true}]),
    gs:button(W,[{x,10},{y,Height-35},
		 {data, {leave, Name}},{label,{text,"Leave Group"}}]),
    {W, T1}.

send_self(Msg, Pid) ->    
    Pid ! {event, ubf:deabstract(Msg)},
    fun(I) -> send_self(I, Pid) end.
	    

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
