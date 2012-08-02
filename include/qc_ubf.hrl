%%% The MIT License
%%%
%%% Copyright (C) 2011-2012 by Joseph Wayne Norton <norton@alum.mit.edu>
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

-ifndef(qc_ubf).
-define(qc_ubf, true).

%% boilerplate API
%% -export([qc_run/1, qc_run/2, qc_run/3]).
%% -export([qc_sample/1, qc_sample/2, qc_prop/1, qc_prop/2]).
%% -export([qc_counterexample/0, qc_counterexample/1, qc_counterexample/2]).
%% -export([qc_counterexample_read/1]).

-include_lib("qc/include/qc_statem.hrl").
-include("ubf.hrl").

%%%=========================================================================
%%%  boilerplate API
%%%=========================================================================

%% -spec qc_run([module()]) -> boolean().
%% qc_run(Contracts) ->
%%     qc_run(Contracts, 500).

%% -spec qc_run([module()], non_neg_integer()) -> boolean().
%% qc_run(Contracts, NumTests) ->
%%     qc_run(Contracts, NumTests, []).

%% -spec qc_run([module()], non_neg_integer(), [{name,string()} | cover | {cover,[module()]} | parallel | noshrink | {sometimes,pos_integer()} | any()]) -> boolean().
%% qc_run(Contracts, NumTests, Options) ->
%%     Mod = qc_ubf:new(?MODULE, Contracts),
%%     Mod:ubf_run(NumTests, Options).

%% %% sample
%% qc_sample(Contracts) ->
%%     qc_sample(Contracts, []).

%% qc_sample(Contracts, Options) ->
%%     Mod = qc_ubf:new(?MODULE, Contracts),
%%     Mod:ubf_sample(Options).

%% %% prop
%% qc_prop(Contracts) ->
%%     qc_prop(Contracts, []).

%% qc_prop(Contracts, Options) ->
%%     Mod = qc_ubf:new(?MODULE, Contracts),
%%     Mod:ubf_prop(Options).

%% %% counterexample
%% qc_counterexample() ->
%%     qc_counterexample([]).

%% qc_counterexample(Options) ->
%%     qc_counterexample(Options, ?QC:counterexample()).

%% qc_counterexample(Options, CounterExample) ->
%%     ?QC:check(qc_prop(Options), CounterExample).

%% %% counterexample read
%% qc_counterexample_read(FileName) ->
%%     {ok, [CounterExample]} = file:consult(FileName),
%%     qc_counterexample([], CounterExample).

-endif. %% -ifdef(qc_ubf).
