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
%%% Copyright (c) 2009-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : qc_ubf_gen_tests.erl
%%% Purpose : GMT gen QuickCheck tests
%%%-------------------------------------------------------------------

-module(qc_ubf_gen_tests).

-ifdef(QC).

-include_lib("qc/include/qc.hrl").

-export([run/0]).
-compile(export_all).

%% run from eunit
eunit_test_() ->
    qc:eunit_module(?MODULE, 3000).

run() ->
    run(3000).

run(NumTests) ->
    qc:module({numtests,NumTests}, ?MODULE).

%% @desc test the any generator against the ubf encoder/decoder
prop_ubf_gen_any() ->
    ?FORALL(X, qc_gen:qc_any(),
            begin
                UBF = ubf:encode(X),
                {done, Y, "", undefined} = ubf:decode(UBF),
                Res = X =:= Y,
                ?WHENFAIL(io:format("~n~p:~p ~p -> ~p -> ~p~n",[?FILE, ?LINE, X, UBF, Y]), Res)
            end).

-endif. %% -ifdef(QC).
