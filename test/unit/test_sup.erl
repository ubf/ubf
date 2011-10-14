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

%%%----------------------------------------------------------------------
%%% File    : test_sup.erl
%%% Purpose : test UBF top-level supervisor
%%%----------------------------------------------------------------------

-module(test_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(_Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%----------------------------------------------------------------------
%% @spec(Args::term()) -> {ok, {supervisor_flags(), child_spec_list()}}
%% @doc The main TEST UBF supervisor.

init(Args) ->
    %% seq_trace:set_token(send, true), seq_trace:set_token('receive', true),

    %% Child_spec = [Name, {M, F, A},
    %%               Restart, Shutdown_time, Type, Modules_used]

    DefaultMaxConn = 10000,
    DefaultTimeout = 60000,
    DefaultPlugins = proplists:get_value(plugins, Args, [file_plugin, irc_plugin, irc_plugin, test_plugin]),

    CUBF = case proplists:get_value(test_ubf_tcp_port, Args, 0) of
               undefined ->
                   [];
               UBFPort ->
                   UBFMaxConn = proplists:get_value(test_ubf_maxconn, Args, DefaultMaxConn),
                   UBFIdleTimer = proplists:get_value(test_ubf_timeout, Args, DefaultTimeout),
                   UBFOptions = [{serverhello, "test_meta_server"}
                                 , {statelessrpc,false}
                                 , {proto,ubf}
                                 , {maxconn,UBFMaxConn}
                                 , {idletimer,UBFIdleTimer}
                                 , {registeredname,test_ubf_tcp_port}
                                ],
                   UBFServer =
                       {ubf_server, {ubf_server, start_link, [undefined, DefaultPlugins, UBFPort, UBFOptions]},
                        permanent, 2000, worker, [ubf_server]},

                   [UBFServer]
           end,

    CEBF = case proplists:get_value(test_ebf_tcp_port, Args, 0) of
               undefined ->
                   [];
               EBFPort ->
                   EBFMaxConn = proplists:get_value(test_ebf_maxconn, Args, DefaultMaxConn),
                   EBFIdleTimer = proplists:get_value(test_ebf_timeout, Args, DefaultTimeout),
                   EBFOptions = [{serverhello, "test_meta_server"}
                                 , {statelessrpc,false}
                                 , {proto,ebf}
                                 , {maxconn,EBFMaxConn}
                                 , {idletimer,EBFIdleTimer}
                                 , {registeredname,test_ebf_tcp_port}
                                ],
                   EBFServer =
                       {ebf_server, {ubf_server, start_link, [undefined, DefaultPlugins, EBFPort, EBFOptions]},
                        permanent, 2000, worker, [ebf_server]},

                   [EBFServer]
           end,

    {ok, {{one_for_one, 2, 60}, CUBF ++ CEBF}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
