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

-include_lib("qc/include/qc_statem.hrl").

%% API
-export([qc_run/4]).
-export([qc_sample/3]).
-export([qc_prop/3]).
-export([qc_counterexample_read/4]).
-export([impl/2]).

%% Interface Functions
-ifndef(old_callbacks).

-type call() :: {call, Mod::atom(), Fun::atom(), Args::list(term())}.
-type var() :: {var, integer()}.

-callback command_contract(SymState::term(), [Contract::atom()]) -> Contract::atom().
-callback scenario_gen() -> Gen::term().
-callback command_typename(SymState::term(), Contract::atom(), [TypeName::atom()]) -> TypeName::atom().
-callback command_typegen(GenFun::fun(), SymState::term(), Contract::atom(), TypeName::atom(), TypeStack::[TypeName::atom()]) -> Gen::term().
-callback command_gen(SymState::term()) -> Gen::term().
-callback command_gen_custom(GenFun::fun(), SymState::term()) -> Gen::term().
-callback initial_state(Scenario::term()) -> SymState::term().
-callback state_is_sane(DynState::term()) -> boolean().
-callback next_state(SymState::term(), R::var(), C::call()) -> SymState::term().
-callback precondition(SymState::term(), C::call()) -> boolean().
-callback postcondition(DynState::term(), C::call(), R::term()) -> boolean().
-callback rpc(Contract::atom(),TypeName::atom(),Type::term()) -> term().
-callback setup() -> ok.
-callback setup(Scenario::term()) -> {ok, Ref::term()}.
-callback teardown(Ref::term(), DynState::term() | undefined) -> ok.
-callback aggregate([{N::integer(), Call::term(), R::term(), DynState::term()}]) -> [term()].

-else. % -ifndef(old_callbacks).

-export([behaviour_info/1]).

%% Define the behaviour's required mods.
behaviour_info(callbacks) ->
    [{command_contract,2}
     , {scenario_gen,0}
     , {command_typename,3}
     , {command_typegen,6}
     , {command_gen,4}
     , {command_gen_custom,2}
     , {initial_state,1}
     , {state_is_sane,1}
     , {next_state,3}
     , {precondition,2}
     , {postcondition,3}
     , {rpc,3}
     , {setup,0}
     , {setup,1}
     , {teardown,2}
     , {aggregate,1}
    ];
behaviour_info(_Other) ->
	undefined.

-endif. % -ifndef(old_callbacks).

%%%=========================================================================
%%%  Records, Types, Macros
%%%=========================================================================

%%%=========================================================================
%%%  API
%%%=========================================================================
qc_run(Mod, Contracts, NumTests, Options) ->
    (impl(Mod, Contracts)):qc_run(NumTests, Options).

qc_sample(Mod, Contracts, Options) ->
    (impl(Mod, Contracts)):qc_sample(Options).

qc_prop(Mod, Contracts, Options) ->
    (impl(Mod, Contracts)):qc_prop(Options).

qc_counterexample_read(Mod, Contracts, Options, FileName) ->
    {ok, [CounterExample]} = file:consult(FileName),
    ?QC:check(qc_prop(Mod, Contracts, Options), CounterExample).

%%%========================================================================
%%% Internal functions
%%%========================================================================
impl(Mod, Contracts) ->
    qc_ubf_impl:new(Mod, Contracts).

-endif. %% -ifdef(QC).
