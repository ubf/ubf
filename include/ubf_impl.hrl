%%% The MIT License
%%%
%%% Copyright (C) 2011-2012 by Joseph Wayne Norton <norton@alum.mit.edu>
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
%%% Description: UBF Implementation Utilities
%%%----------------------------------------------------------------------

-ifndef(ubf_impl).
-define(ubf_impl, true).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------

-define(UBF_TLOG_MODULE_DEFAULT, undefined).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------

-type contract_name() :: nonempty_string().
-type contract_vsn() :: nonempty_string().
-type contract_types() :: [term()].
-type contract_typenames() :: [atom()].
-type contract_records() :: [term()].
-type contract_transitions() :: [term()].
-type contract_anystate() :: [term()].

-record(contract, {
          name :: contract_name(),
          vsn :: contract_vsn(),
          types=[] :: contract_types(),
          leaftypenames=[] :: contract_typenames(),
          importtypenames=[] :: contract_typenames(),
          records=[] :: contract_records(),
          transitions=[] :: contract_transitions(),
          anystate=[] :: contract_anystate()
         }).

-endif. % -ifndef(ubf_impl)
