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

%%% @doc Keeps track of a number of TCP sessions.
%%%
%%% This module will manage a collection of TCP sessions for the same
%%% server.  If +Port+ is 0, the underlying OS assigns an available
%%% port number.  If a +Name+ is not specified, the server will be
%%% named +picoSocketServer_+ plus the TCP port number that the
%%% service listens to, e.g. +picoSocketServer_9923+.
%%%
%%% A managed server can be started, stopped, enumerate child
%%% sessions, and limit the maximum number of child sessions.
%%%
%%% The conventions used by this module look quite different than
%%% OTP-based services, due to its origin.

-module(proc_socket_server).

%%% Copyright (C) 1999, Bluetail AB
%%% File    : proc_socket_server.erl
%%% Author  : Joe Armstrong (joe@bluetail.com)

-export([start_raw_server/5, start_raw_server/7, start_server/3, start_server/4, stop_server/1]).

-export([server_port/1, server_port/2, server_status/1, server_status/2, server_children/1, server_children/2]).

%% Internal imports (used by spawn)

-export([cold_start/7, start_child/3]).

%% @doc Start a new UBF contract-using server.
%%
%% - This server accepts up to Max connections on TCP port Port.
%% - SpawnOpts are the erlang garbage collection options for the spawned process.
%% - Each time a new connection is made, Fun(Socket) is called.
%%
%% ------
%% Fun = fun(Socket) -> exit(normal) | exit(socket_closed) |
%%                      exit({socket_error, Reason}) | exit(timeout).
%% ------
%% Fun will handle all of the protocol communication for a single TCP
%% session.
%%
%% A raw server uses packet length 0 (see start_raw_server/5 and
%% start_raw_server/7).

start_server(Port, Max, Fun) ->
    start_raw_server(undefined, Port, Max, [], Fun, 0, 0).

start_server(Name, Port, Max, Fun) ->
    start_raw_server(Name, Port, Max, [], Fun, 0, 0).

start_raw_server(Port, Max, Fun, PacketType, PacketSize) ->
    start_raw_server(undefined, Port, Max, [], Fun, PacketType, PacketSize).

start_raw_server(undefined, Port, Max, SpawnOpts, Fun, PacketType, PacketSize) when Port =/= 0 ->
    Name = port_name(Port),
    start_raw_server(Name, Port, Max, SpawnOpts, Fun, PacketType, PacketSize);
start_raw_server(Name, Port, Max, SpawnOpts, Fun, PacketType, PacketSize) ->
    case whereis(Name) of
        undefined ->
            Parent = self(),
            Pid = erlang:spawn_opt(?MODULE, cold_start,
                                   [Parent, Name, Port, Max, Fun, PacketType, PacketSize],
                                   [link | SpawnOpts]),
            {ok, Pid};
        _Pid ->
            false
    end.

stop_server(Pid) when is_pid(Pid) ->
    exit(Pid, kill),
    ok;
stop_server(Port) when is_integer(Port) ->
    Name = port_name(Port),
    stop_server(Name);
stop_server(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            ok = stop_server(Pid),
            (catch unregister(Name)),
            ok
    end.


server_port(Name) ->
    server_port(Name, 16#ffffffff).

server_port(Pid, Timeout) when is_pid(Pid) ->
    Pid ! {port, self()},
    receive
        {Pid, Reply} ->
            Reply
    after Timeout ->
            timeout
    end;
server_port(Port, Timeout) when is_integer(Port) ->
    Name = port_name(Port),
    server_port(Name, Timeout);
server_port(Name, Timeout) ->
    case whereis(Name) of
        undefined ->
            retrylater;
        Pid ->
            server_port(Pid, Timeout)
    end.


server_status(Name) ->
    server_status(Name, 16#ffffffff).

server_status(Pid, Timeout) when is_pid(Pid) ->
    Pid ! {status, self()},
    receive
        {Pid, Reply} ->
            Reply
    after Timeout ->
            timeout
    end;
server_status(Port, Timeout) when is_integer(Port) ->
    Name = port_name(Port),
    server_status(Name, Timeout);
server_status(Name, Timeout) ->
    case whereis(Name) of
        undefined ->
            retrylater;
        Pid ->
            server_status(Pid, Timeout)
    end.


server_children(Name) ->
    server_children(Name, 16#ffffffff).

server_children(Pid, Timeout) when is_pid(Pid) ->
    Pid ! {children, self()},
    receive
        {Pid, Reply} ->
            Reply
    after Timeout ->
            timeout
    end;
server_children(Port, Timeout) when is_integer(Port) ->
    Name = port_name(Port),
    server_children(Name, Timeout);
server_children(Name, Timeout) ->
    case whereis(Name) of
        undefined ->
            retrylater;
        Pid ->
            server_children(Pid, Timeout)
    end.


cold_start(Parent, Name, Port, Max, Fun, PacketType, PacketSize) ->
    process_flag(trap_exit, true),
    DefaultListenOptions =
        [binary, {nodelay, true}, {active, false}, {reuseaddr, true}, {backlog, 4096}],
    ListenOptions =
        case {PacketType, PacketSize} of
            {0,0} ->
                DefaultListenOptions;
            {T,0} ->
                DefaultListenOptions++[{packet,T}];
            {0,S} ->
                DefaultListenOptions++[{packet_size,S}];
            {T,S} ->
                DefaultListenOptions++[{packet,T}, {packet_size,S}]
        end,
    {ok, Listen} = gen_tcp:listen(Port, ListenOptions),
    RegisterName =
        if Name =/= undefined ->
                Name;
           true ->
                {ok, RealPort} = inet:port(Listen),
                port_name(RealPort)
        end,
    register(RegisterName, self()),
    New = start_accept(Listen, Fun),
    socket_loop(Parent, Listen, New, [], 0, Max, Fun).


port_name(Port) when is_integer(Port) ->
    list_to_atom("picoSocketServer_" ++ integer_to_list(Port)).


%% Don't mess with the following code unless you really know what
%% you're doing (and Thanks to Magnus for heping me get it right)

socket_loop(Parent, Listen, New, Active, Num, Max, Fun) ->
    receive
        {started, New} ->
            Active1 = [New|Active],
            possibly_start_another(Parent, false, Listen, Active1, Num+1, Max, Fun);
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        {'EXIT', New, _Reason} ->
            possibly_start_another(Parent, false, Listen, Active, Num, Max, Fun);
        {'EXIT', Pid, _Reason} ->
            Active1 = lists:delete(Pid, Active),
            possibly_start_another(Parent, New, Listen, Active1, length(Active1), Max, Fun);
        {port, From} ->
            {ok, Port} = inet:port(Listen),
            From ! {self(), Port},
            socket_loop(Parent, Listen, New, Active, Num, Max, Fun);
        {status, From} ->
            From ! {self(), {Num, Max}},
            socket_loop(Parent, Listen, New, Active, Num, Max, Fun);
        {children, From} ->
            From ! {self(), Active},
            socket_loop(Parent, Listen, New, Active, Num, Max, Fun);
        Other ->
            io:format("*** YY Socket loop dropping:~p~n", [Other]),
            socket_loop(Parent, Listen, New, Active, Num, Max, Fun)
    end.


possibly_start_another(Parent, New, Listen, Active, Num, Max, Fun) when is_pid(New) ->
    socket_loop(Parent, Listen, New, Active, Num, Max, Fun);
possibly_start_another(Parent, false, Listen, Active, Num, Max, Fun) ->
    if Num < Max ->
            New = start_accept(Listen, Fun),
            socket_loop(Parent, Listen, New, Active, Num, Max, Fun);
       true ->
            socket_loop(Parent, Listen, false, Active, Num, Max, Fun)
    end.


start_accept(Listen, Fun) ->
    Parent = self(),
    erlang:spawn_link(?MODULE, start_child, [Parent, Listen, Fun]).


start_child(Parent, Listen, Fun) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            %% Kick off the parent
            Parent ! {started, self()},
            %% Start the child
            case (catch Fun(Socket)) of
                {'EXIT', normal} ->
                    true;
                {'EXIT', socket_closed} ->
                    true;
                {'EXIT', Reason} ->
                    gen_tcp:close(Socket),
                    exit(Reason)
            end;
        Other ->
            exit(Other)
    end.

