%%% The MIT License
%%%
%%% Copyright (C) 2011 by Joseph Wayne Norton <norton@alum.mit.edu>
%%% Copyright (C) 2002 by Joe Armstrong
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(irc_client_gs).

-include("ubf.hrl").

-compile(export_all).

-import(ubf_client, [rpc/2]).

%% Prerequisites:
%%
%% 1. The current working directory must be "..", because test() will
%%    try to access the file fileNameToGet(), which exists in our
%%    parent directory.  (Or else create a symbolic link with that
%%    name that points to the file in "..".
%%
%% 2. A UBF server must be listening to TCP port defaultPort() and
%%    have the "irc" contract, which is implemented by the
%%    "irc_plugin" module.
%%
%% Here is minimal recipe.
%%
%%   erl -pz ../../ebin
%%
%%   > ubf_server:start([irc_plugin], 2000, []).
%%   > irc_client_gs:start("TestNickName").

batch([Nick]) ->
    start(atom_to_list(Nick)).

start(Nick) ->
    spawn(fun() -> init1(Nick) end).

init1(Nick) ->
    case ubf_client:connect("localhost", 2000) of
        {ok, Pid, _Name} ->
            {reply, {ok,_},_} = ubf_client:rpc(Pid, {startSession, ?S("irc"),
                                                 []}),
            ubf_client:install_handler(Pid, fun print_msg/1),
            {reply, _, _} = rpc(Pid, logon),
            Self = self(),
            ubf_client:install_handler(Pid, fun(M) ->
                                                send_self(M, Self)
					    end),
            case rpc(Pid, {nick, ?S(Nick)}) of
                {reply, false, _} ->
                    ubf_client:stop(Pid),
                    io:format("Nick was in use try again~n"),
                    erlang:halt();
                {reply, true, active} ->
                    init(Pid, Nick)
            end,
            io:format("client stops~n");
        {error, socket} ->
            io:format("Cannot make TCP connection~n"),
            erlang:halt()
    end.

init(Pid, Nick) ->
    S=gs:start(),
    Width=250,Height=170,
    W= gs:window(S,[{title,"IRC client"},
                 {width,Width},{height,Height},{map,true}]),
    L1 = gs:label(W, [{x,10},{y,10},{label,{text,"Nick:" ++ Nick}}]),
    E1=gs:entry(W, [{x,10},{y,40},{width, 120}]),
    gs:button(W,[{x,130},{y,40},{data, join},{label,{text,"Join Group"}}]),
    gs:config(E1, {text, "erlang"}),
    E2=gs:entry(W, [{x,10},{y,70},{width, 120}]),
    gs:button(W,[{x,130},{y,70}, {data,nick},{label,{text,"Change Nick"}}]),
    gs:button(W,[{x,10},{y,110}, {data,quit}, {label,{text,"Quit"}}]),
    loop(S, dict:new(), Pid, L1, E1,E2).

loop(S, Dict, Pid, L1, E1, E2) ->
    receive
        {gs,_,click,nick,_} ->
            Nick = gs:read(E2, text),
            io:format("Change nick to:~s~n",[Nick]),
            case rpc(Pid, {nick, ?S(Nick)}) of
                {reply, false, _} ->
                    gs:config(E2, {text, "** bad nick **"}),
                    loop(S, Dict, Pid, L1, E1, E2);
                {reply, true, active} ->
                    io:format("nick was changed~n"),
                    gs:config(L1, {label, {text, "Nick: " ++ Nick}}),
                    loop(S, Dict, Pid, L1, E1, E2)
            end,
            loop(S, Dict, Pid, L1, E1, E2);
        {gs,_,click,{leave,Group},_} ->
            io:format("I leave: ~s~n",[Group]),
            case dict:find(Group, Dict) of
                {ok, {W,_}} ->
                    gs:destroy(W),
                    Dict1 = dict:erase(Group, Dict),
                    rpc(Pid, {leave, ?S(Group)}),
                    loop(S, Dict1, Pid, L1, E1, E2);
                error ->
                    loop(S, Dict, Pid, L1, E1, E2)
            end;
        {gs,_,click,join,_} ->
            Group = gs:read(E1, text),
            io:format("Join:~s~n",[Group]),
            case Group of
                "" ->
                    loop(S, Dict, Pid, L1, E1, E2);
                _ ->
                    case dict:find(Group, Dict) of
                        {ok, _W} ->
                            loop(S, Dict, Pid, L1, E1, E2);
                        error ->
                            W = new_group(S, Group),
                            rpc(Pid, {join, ?S(Group)}),
                            loop(S, dict:store(Group, W, Dict), Pid,
                                 L1, E1, E2)
                    end
            end;
        {gs,_,click,quit,_} ->
            erlang:halt();
        {gs,Obj,keypress,G,['Return'|_]} ->
            Str = gs:read(Obj, text),
            gs:config(Obj, {text, ""}),
            io:format("Send: ~s to ~s~n",[Str, G]),
            rpc(Pid, {msg, ?S(G), ?S(Str ++ "\n")}),
            loop(S, Dict, Pid, L1, E1, E2);
        {gs,_Obj,keypress,_G,_} ->
            loop(S, Dict, Pid, L1, E1, E2);
        {event_out, {leaves, Who, Group}} ->
            display(Group, Dict, Who  ++ " leaves the group\n"),
            loop(S, Dict, Pid, L1, E1, E2);
        {event_out, {joins, Who, Group}} ->
            display(Group, Dict, Who  ++ " joins the group\n"),
            loop(S, Dict, Pid, L1, E1, E2);
        {event_out, {changesName, Old, New, Group}} ->
            display(Group, Dict, Old ++ " changes name to " ++ New ++ "\n"),
            loop(S, Dict, Pid, L1, E1, E2);
        {event_out, {msg, From, Group, Msg}} ->
            display(Group, Dict, From ++ " > " ++ Msg),
            loop(S, Dict, Pid, L1, E1, E2);
        X ->
            io:format("man: got other: ~w~n",[X]),
            loop(S, Dict, Pid, L1, E1, E2)
    end.

display(Group, Dict, Str) ->
    case dict:find(Group, Dict) of
        {ok, {_W, Txt}} ->
            gs:config(Txt, {insert, {'end', Str}});
        error ->
            io:format("Cannot display:~s ~s~n",[Group, Str])
    end.

new_group(S, Name) ->
    Width=450,Height=350,
    W  = gs:window(S,[{title,"Name"},
                    {width,Width},{height,Height},{map,true}]),
    _L1 = gs:label(W, [{x,10},{y,10},{label,{text,"Group:" ++ Name}}]),
    T1 = gs:editor(W,  [{x,10},{y,40},
                        {width,Width-20},
                        {height,Height-120},
                        {vscroll, right}]),
    _E1 = gs:entry(W, [{x,10},{y,Height-70},{width, Width-20},
                      {data, Name},
                      {keypress,true}]),
    gs:button(W,[{x,10},{y,Height-35},
                 {data, {leave, Name}},{label,{text,"Leave Group"}}]),
    {W, T1}.

send_self(Msg, Pid) ->
    Pid ! {event_out, ubf:deabstract(Msg)},
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
