%%% The MIT License
%%%
%%% Copyright (C) 2011-2015 by Joseph Wayne Norton <norton@alum.mit.edu>
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

-include_lib("pmod_transform/include/pmod.hrl").

-module(qc_ubf_impl, [MOD, CONTRACTS]).

-ifdef(QC).

%% API
-export([qc_run/2]).
-export([qc_sample/1]).
-export([qc_prop/1]).

%% Misc API
-export([rpc/3]).

%% qc_statem Callbacks
-behaviour(qc_statem).
-export([scenario_gen/0, command_gen/1, command_gen/2, command_typegen/4]).
-export([initial_state/1, state_is_sane/1, next_state/3, precondition/2, postcondition/3]).
-export([setup/0, setup/1, teardown/2, aggregate/1]).

-include_lib("qc/include/qc_statem.hrl").

%%%=========================================================================
%%%  Records, Types, Macros
%%%=========================================================================
-define(UBF_DEFAULT_INPUT_TYPENAMES,
        [ubf_info_req,ubf_description_req,ubf_contract_req,ubf_keepalive_req]).

-define(UBF_DEFAULT_OUTPUT_TYPENAMES,
        [ubf_info_res,ubf_description_res,ubf_contract_res,ubf_keepalive_res]).

%%%=========================================================================
%%%  API
%%%=========================================================================
qc_run(NumTests, Options) ->
    qc_statem:qc_run(THIS, NumTests, Options ++ [{name,MOD}]).

qc_sample(Options) ->
    qc_statem:qc_sample(THIS, Options).

qc_prop(Options) ->
    qc_statem:qc_prop(THIS, Options).

%%%=========================================================================
%%%  Callbacks - eqc_statem
%%%=========================================================================

%% scenario generator
scenario_gen() ->
    try
        MOD:scenario_gen()
    catch
        error:undef ->
            undefined
    end.

%% command generator
command_gen(S) ->
    command_gen(S, input).

command_gen(S, IO) ->
    Gen = fun try_command_typegen/4,
    ?LET(Contract,try_command_contract(S, CONTRACTS),
         if Contract =/= undefined ->
                 TypeNames = extract_typenames(Contract, IO),
                 ?LET(TypeName,try_command_typename(S, Contract, TypeNames),
                      try_command_gen(Gen, S, Contract, TypeName));
            true ->
                 MOD:command_gen_custom(Gen, S)
         end).

try_command_contract(S, Contracts) ->
    try
        MOD:command_contract(S, Contracts)
    catch
        error:undef ->
            oneof(Contracts)
    end.

try_command_typename(S, Contract, TypeNames) ->
    try
        MOD:command_typename(S, Contract, TypeNames)
    catch
        error:undef ->
            oneof(TypeNames)
    end.

try_command_gen(Gen, S, Contract, TypeName) ->
    try
        MOD:command_gen(Gen, S, Contract, TypeName)
    catch
        error:undef ->
            ?LET(Type, Gen(S, Contract, TypeName, []),
                 {call,THIS,rpc,[Contract,TypeName,Type]})
    end.

try_command_typegen(S, Contract, TypeName, TypeStack) ->
    Gen = fun command_typegen/4,
    try
        MOD:command_typegen(Gen, S, Contract, TypeName, TypeStack)
    catch
        error:undef ->
            Gen(S, Contract, TypeName, TypeStack)
    end.

command_typegen(S, Contract, TypeName, TypeStack) ->
    Fun = fun(TN) -> try_command_typegen(S, Contract, TN, [TypeName|TypeStack]) end,
    Gen =
        fun(TN) ->
                case lists:member(TypeName, TypeStack) of
                    true ->
                        ?SIZED(Size, resize(round(math:sqrt(Size)), Fun(TN)));
                    false ->
                        Fun(TN)
                end
        end,
    qc_ubf_types:type(Gen, Contract, TypeName).

%% initial state
initial_state(Scenario) ->
    try
        MOD:initial_state(Scenario)
    catch
        error:undef ->
            undefined
    end.

%% state is sane
state_is_sane(S) ->
    try
        MOD:state_is_sane(S)
    catch
        error:undef ->
            true
    end.

%% next state
next_state(S,R,C) ->
    try
        MOD:next_state(S,R,C)
    catch
        error:undef ->
            S
    end.

%% precondition
precondition(S,C) ->
    try
        MOD:precondition(S,C)
    catch
        error:undef ->
            true
    end.

%% postcondition
postcondition(_S,_C,{clientBrokeContract,_,_}) ->
    false;
postcondition(_S,_C,{serverBrokeContract,_,_}) ->
    false;
postcondition(S,C,R) ->
    try
        MOD:postcondition(S,C,R)
    catch
        error:undef ->
            true
    end.

%% rpc
rpc(Contract,TypeName,Type) ->
    try
        MOD:rpc(Contract, TypeName, Type)
    catch
        error:undef ->
            case ubf_client:lpc(Contract, Type) of
                {reply,Reply,_State} ->
                    Reply;
                Err ->
                    erlang:error(Err)
            end
    end.

%% setup
setup() ->
    try
        MOD:setup()
    catch
        error:undef ->
            ok
    end.

setup(Scenario) ->
    try
        MOD:setup(Scenario)
    catch
        error:undef ->
            {ok,undefined}
    end.

%% teardown
teardown(Ref, State) ->
    try
        MOD:teardown(Ref, State)
    catch
        error:undef ->
            ok
    end.

%% aggregate
aggregate(L) ->
    try
        MOD:aggregate(L)
    catch
        error:undef ->
            []
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
extract_typenames(Contract,input) ->
    [ Input
      || {{prim,1,1,Input},{prim,1,1,_Output}} <- Contract:contract_anystate(),
         not lists:member(Input, ?UBF_DEFAULT_INPUT_TYPENAMES) ];
extract_typenames(Contract,output) ->
    [ Output
      || {{prim,1,1,_Input},{prim,1,1,Output}} <- Contract:contract_anystate(),
         not lists:member(Output, ?UBF_DEFAULT_OUTPUT_TYPENAMES) ].

-endif. %% -ifdef(QC).
