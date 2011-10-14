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

%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% File    : qc_ubf.erl
%%% Purpose : QuickCheck wrappers for UBF
%%%-------------------------------------------------------------------

-module(qc_ubf).

-ifdef(QC).

-include_lib("qc/include/qc.hrl").

-include("ubf.hrl").

%% API
-export([ubf_sample_commands/2, ubf_sample_commands/3]).
-export([ubf_run_commands/2, ubf_run_commands/3]).
-export([ubf_gen_command/5, ubf_gen_command_type/5]).

%% eqc_statem Callbacks
-export([command/1, initial_state/0, initial_state/1, next_state/3, precondition/2, postcondition/3]).

%% Interface Functions
-export([behaviour_info/1]).

%% Define the behaviour's required mods.
behaviour_info(callbacks) ->
    [{ubf_command_contract,2}
     , {ubf_command_typename,3}
     , {ubf_command_typegen,6}
     , {ubf_command,5}
     , {ubf_command_custom,3}
     , {ubf_rpc,3}
     , {ubf_initial_state,0}
     , {ubf_state_is_sane,1}
     , {ubf_next_state,3}
     , {ubf_precondition,2}
     , {ubf_postcondition,3}
     , {ubf_commands_setup,1}
     , {ubf_commands_teardown,1}
     , {ubf_commands_teardown,2}
    ].

-define(UBF_DEFAULT_TYPENAMES,
        [info_req,description_req,contract_req,keepalive_req]).

%%%----------------------------------------------------------------------
%%% records
%%%----------------------------------------------------------------------

%%%%%%
%% state
-record(state,
        {
          %% mod
              mod
              %% mod state
              , mod_state
        }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

ubf_sample_commands(Mod, Contracts) ->
    ubf_sample_commands(Mod, Contracts, []).

ubf_sample_commands(Mod, Contracts, Options)
  when is_atom(Mod), is_list(Contracts), is_list(Options) ->
    %% commands - sample
    Params = [{ubfmod,Mod},{ubfcontracts,Contracts},{ubfoptions,Options}],
    ?QC_GEN:sample(with_parameters(Params,
                                   ?LET(InitialState,initial_state(Mod),
                                        command(InitialState)))).

ubf_run_commands(Mod, Contracts) ->
    ubf_run_commands(Mod, Contracts, []).

ubf_run_commands(Mod, Contracts, Options)
  when is_atom(Mod), is_list(Contracts), is_list(Options) ->
    %% commands - setup and teardown
    {ok,TestRefOnce} = Mod:ubf_commands_setup(true),
    ok = Mod:ubf_commands_teardown(TestRefOnce),

    %% commands - loop
    Parallel = proplists:get_bool(parallel, Options),
    Params = [{parallel,Parallel},{ubfmod,Mod},{ubfcontracts,Contracts},{ubfoptions,Options}],
    case Parallel of
        false ->
            ?FORALL(Cmds,with_parameters(Params,
                                         ?LET(InitialState,initial_state(Mod),
                                              commands(?MODULE,InitialState))),
                    begin
                        %% commands - setup
                        {ok,TestRef} = Mod:ubf_commands_setup(false),

                        %% commands - run
                        {H,S,Res} = run_commands(?MODULE,Cmds,Params),

                        %% whenfail
                        ?WHENFAIL(
                           begin
                               %% commands
                               FileName = write_commands(Cmds),
                               io:format("~nCOMMANDS:~n\t~p~n",[FileName]),
                               %% history
                               io:format("~nHISTORY:"),
                               if
                                   length(H) < 1 ->
                                       io:format(" none~n");
                                   true ->
                                       CmdsH = zip(tl(Cmds),H),
                                       [ begin
                                             {Cmd,{State,Reply}} = lists:nth(N,CmdsH),
                                             io:format("~n #~p:~n\tCmd: ~p~n\tReply: ~p~n\tState: ~p~n",
                                                       [N,Cmd,Reply,State])
                                         end
                                         || N <- lists:seq(1,length(CmdsH)) ]
                               end,
                               %% result
                               io:format("~nRESULT:~n\t~p~n",[Res]),
                               %% state
                               io:format("~nSTATE:~n\t~p~n",[S]),
                               %% state is sane
                               io:format("~nSTATE IS SANE:~n\t~p~n",[state_is_sane(Mod, S)])
                           end,
                           (ok =:= Res
                            andalso state_is_sane(Mod, S)
                            %% commands - teardown
                            andalso ok =:= Mod:ubf_commands_teardown(TestRef,S#state.mod_state)))
                    end);
        true ->
            ?FORALL(_Repetitions,?SHRINK(1,[10]),
                    ?FORALL(Cmds,with_parameters(Params,
                                                 ?LET(InitialState,initial_state(Mod),
                                                      parallel_commands(?MODULE,InitialState))),
                            ?ALWAYS(_Repetitions,
                                    begin
                                        %% commands - setup
                                        {ok,TestRef} = Mod:ubf_commands_setup(false),

                                        %% commands - run
                                        {H,HL,Res} = run_parallel_commands(?MODULE,Cmds,Params),

                                        %% whenfail
                                        ?WHENFAIL(
                                           begin
                                               %% commands
                                               FileName = write_commands(Cmds),
                                               io:format("~nCOMMANDS:~n\t~p~n",[FileName]),
                                               %% history
                                               io:format("~nHISTORY:~n\t~p~n",[H]),
                                               %% history list
                                               io:format("~nHISTORY LIST:~n\t~p~n",[HL]),
                                               %% result
                                               io:format("~nRESULT:~n\t~p~n",[Res])
                                           end,
                                           (ok =:= Res
                                            %% commands - teardown
                                            andalso ok =:= Mod:ubf_commands_teardown(TestRef,undefined)))
                                    end)))
    end;
ubf_run_commands(_Mod, _Contracts, _Options) ->
    exit(badarg).

ubf_gen_command(Mod, ModState, Contract, TypeName, TypeStack) ->
    Mod:ubf_command_typegen(fun ubf_gen_command_type/5, Mod, ModState, Contract, TypeName, TypeStack).

ubf_gen_command_type(Mod, ModState, Contract, TypeName, TypeStack) ->
    Gen = fun(TN) ->
                  case lists:member(TypeName, TypeStack) of
                      true ->
                          ?SIZED(Size,resize(round(math:sqrt(Size)),
                                             Mod:ubf_command_typegen(fun ubf_gen_command_type/5, Mod, ModState, Contract, TN, [TypeName|TypeStack])));
                      false ->
                          Mod:ubf_command_typegen(fun ubf_gen_command_type/5, Mod, ModState, Contract, TN, [TypeName|TypeStack])
                  end
          end,
    qc_ubf_types:type(Gen, Contract, TypeName).


%%%----------------------------------------------------------------------
%%% Callbacks - eqc_statem
%%%----------------------------------------------------------------------

%% initial state
initial_state() ->
    #state{}.

initial_state(Mod) ->
    #state{mod=Mod, mod_state=Mod:ubf_initial_state()}.

%% state is sane
state_is_sane(Mod, S) ->
    Mod:ubf_state_is_sane(S#state.mod_state).

%% command generator
command(S)
  when is_record(S,state) ->
    ?LET(Contracts,parameter(ubfcontracts),
         %% (S::symbolic_state(),Contracts::list(atom())) -> atom()
         ?LET(Contract,(S#state.mod):ubf_command_contract(S#state.mod_state, Contracts),
              begin
                  if Contract =/= undefined ->
                          InputTypeNames = ubf_extract_input_typenames(Contract),
                          %% (S::symbolic_state(),Contract::atom(),InputTypeNames::list(atom()) -> atom()
                          ?LET(InputTypeName,(S#state.mod):ubf_command_typename(S#state.mod_state, Contract, InputTypeNames),
                               %% (Gen, S::symbolic_state(),Contract::atom(),InputTypeName::atom()) -> gen()
                               (S#state.mod):ubf_command(fun ubf_gen_command/5, S#state.mod, S#state.mod_state, Contract, InputTypeName));
                     true ->
                          (S#state.mod):ubf_command_custom(fun ubf_gen_command/5, S#state.mod, S#state.mod_state)
                  end
              end)).

%% next state
next_state(S,R,C) ->
    NewModState = (S#state.mod):ubf_next_state(S#state.mod_state,R,C),
    S#state{mod_state=NewModState}.

%% precondition
precondition(S,C) ->
    (S#state.mod):ubf_precondition(S#state.mod_state,C).

%% postcondition
postcondition(_S,_C,{clientBrokeContract,_,_}) ->
    false;
postcondition(_S,_C,{serverBrokeContract,_,_}) ->
    false;
postcondition(S,C,R) ->
    (S#state.mod):ubf_postcondition(S#state.mod_state,C,R).


%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

ubf_extract_input_typenames(Contract) ->
    [ Input || {{prim,1,1,Input},{prim,1,1,_Output}}
                   <- Contract:contract_anystate(), not lists:member(Input, ?UBF_DEFAULT_TYPENAMES) ].

write_commands(Cmds) ->
    Module = ?MODULE,
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
    FileName = lists:flatten(io_lib:format("~s-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B.erl",
                                           [Module,Year,Month,Day,Hour,Minute,Second])),
    write_commands(Cmds,FileName).

write_commands(Cmds,FileName) ->
    ok = file:write_file(FileName, io_lib:format("~p.", [Cmds])),
    FileName.

-endif. %% -ifdef(QC).
