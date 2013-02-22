%%% The MIT License
%%%
%%% Copyright (C) 2011-2013 by Joseph Wayne Norton <norton@alum.mit.edu>
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

-module(irc_client).

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
%%   > irc_client:start("TestNickName").
%%
%%   Commands to join and leave groups, etc ... see the loop() function
%%   for details.
%%
%% In another window/login session on the same machine, run another
%% "erl" shell, like the one above.
%%
%%   > irc_client:start("2nd-nick").

batch([Name]) ->
    start(atom_to_list(Name)),
    erlang:halt().

start(Nick) ->
    {ok, Pid, _Name} = ubf_client:connect("localhost", 2000),
    {reply,{ok,_}, _} = rpc(Pid, {startSession, ?S("irc"), []}),
    ubf_client:install_handler(Pid, fun print_msg/1),
    {reply, _, _} = rpc(Pid, logon),
    case rpc(Pid, {nick, ?S(Nick)}) of
        {reply, false, _} ->
            ubf_client:stop(Pid);
        {reply, true, active} ->
            {reply,_,_} = rpc(Pid, {join, ?S("erlang")}),
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
                    {reply, _, _} = rpc(Pid, {join, ?S(G)}),
                    loop(Pid, Group, [G|Gs], Nick);
                ["leave", G] ->
                    rpc(Pid, {leave, ?S(G)}),
                    Gs1 = lists:delete(G, Gs),
                    loop(Pid, Group, Gs1, Nick);
                ["nick", NewNick] ->
                    case rpc(Pid, {nick, ?S(NewNick)}) of
                        {reply, false, _} ->
                            loop(Pid, Group, Gs, Nick);
                        {reply, true, active} ->
                            loop(Pid, Group, Gs, NewNick)
                    end;
                ["quit"] ->
                    ubf_client:stop(Pid),
                    true;
		_ ->
                    %% io:format("OOPs:~p~n",[X]),
                    banner(),
                    loop(Pid, Group, Gs, Nick)

	    end;
        Msg ->
            case rpc(Pid, {msg, ?S(Group), ?S(Msg)}) of
                {reply, _R,_} ->
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
