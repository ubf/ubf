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
%%% File    : qc_ubf_types.erl
%%% Purpose : QuickCheck type generators for UBF
%%%-------------------------------------------------------------------

-module(qc_ubf_types).

-ifdef(QC).

-include_lib("qc/include/qc.hrl").

-include("ubf.hrl").

%% API
-export([type/2, type/3]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% type/3
type(Gen, Contract, TypeName) ->
    case TypeName of
        {predef,_} ->
            Type = TypeName;
        _ ->
            {Type,_} = Contract:contract_type(TypeName)
    end,
    type1(Gen,Type).

type(Gen,Type) ->
    %%io:format("~p~n", [Type]),
    ?SIZED(Size,resize(if Size > 0 -> Size-1; true -> 0 end,type1(Gen,Type))).

%% alt
type1(Gen,{alt,Type,Type2}) ->
    oneof([type1(Gen,Type), type1(Gen,Type2)]);
%% concat
type1(Gen,{concat,Type,Type2}) ->
    ?LET(L1,type1(Gen,Type),
         ?LET(L2,type1(Gen,Type2),
              L1 ++ L2));
%% prim
type1(Gen,{prim,1,1,TypeName}) ->
    Gen(TypeName);
type1(Gen,{prim,0,1,TypeName}) ->
    oneof([undefined,Gen(TypeName)]);
type1(_Gen,{prim,0,0,_Tag}) ->
    undefined;
%% tuple
type1(Gen,{tuple,Elements}) ->
    list_to_tuple([type1(Gen,E) || E <- tuple_to_list(Elements)]);
%% record
type1(Gen,{record,Name,_,_,Elements}) when is_atom(Name) ->
    list_to_tuple([Name|[type1(Gen,E) || E <- tl(tuple_to_list(Elements))]]);
type1(Gen,{record_ext,Name,_,_,Elements}) when is_atom(Name) ->
    list_to_tuple([Name|[type1(Gen,E) || E <- tl(tuple_to_list(Elements))]]);
%% list
type1(Gen,{list,Min,Max,Element}) ->
    repeat(Gen,Min,Max,infinity,Element);
%% range
type1(_Gen,{range,infinity,Hi}) ->
    ?LET(Infinity,largeint(),
         %% @tbd this may not be sufficient
         choose(-1 * abs(Infinity),Hi));
type1(_Gen,{range,Lo,infinity}) ->
    ?LET(Infinity,largeint(),
         %% @tbd this may not be sufficient
         choose(Lo,abs(Infinity)));
type1(_Gen,{range,Lo,Hi}) ->
    choose(Lo,Hi);
%% atom
type1(_Gen,{atom,Value}) when is_atom(Value) ->
    Value;
%% binary
type1(_Gen,{binary,Value}) when is_binary(Value) ->
    Value;
%% float
type1(_Gen,{float,Value}) when is_float(Value) ->
    Value;
%% integer
type1(_Gen,{integer,Value}) when is_integer(Value) ->
    Value;
%% string
type1(_Gen,{string,Value}) when is_list(Value) ->
    Value;
%% predef
type1(_Gen,{predef,any}) ->
    qc_gen:qc_term();
type1(_Gen,{predef,none}) ->
    %% not supported
    exit(fatal);
type1(_Gen,{predef,atom}) ->
    qc_gen:qc_atom();
type1(_Gen,{predef,integer}) ->
    oneof([int(),largeint()]);
type1(_Gen,{predef,float}) ->
    real();
type1(_Gen,{predef,binary}) ->
    qc_gen:qc_binary();
type1(_Gen,{predef,list}) ->
    qc_gen:qc_list();
type1(_Gen,{predef,tuple}) ->
    qc_gen:qc_tuple();
%% predef with attributes
type1(_Gen,{predef,{any,Attrs}}) ->
    qc_gen:qc_term(Attrs);
type1(_Gen,{predef,{atom,Attrs}}) ->
    qc_gen:qc_atom(Attrs);
type1(_Gen,{predef,{binary,Attrs}}) ->
    qc_gen:qc_binary(Attrs);
type1(_Gen,{predef,{list,Attrs}}) ->
    qc_gen:qc_list(Attrs);
type1(_Gen,{predef,{tuple,Attrs}}) ->
    qc_gen:qc_tuple(Attrs);
%% abnf
type1(Gen,{abnf_alt,Types}) ->
    ?LET(T,oneof(Types),
         type1(Gen,T));
type1(Gen,{abnf_seq,Types}) ->
    abnf_type_seq(Gen,Types,[]);
type1(Gen,{abnf_repeat,Min,Max,Element}) ->
    abnf_repeat(Gen,Min,Max,1,Element);
type1(_Gen,{abnf_byte_range,Lo,Hi}) ->
    ?LET(Value,choose(Lo,Hi),
         <<Value:8>>);
type1(Gen,{abnf_byte_alt,Types}) ->
    ?LET(T,oneof(Types),
         type(Gen,T));
type1(Gen,{abnf_byte_seq,Types}) ->
    abnf_type_byte_seq(Gen,Types,[]);
type1(_Gen,{abnf_byte_val,Value}) ->
    <<Value:8>>;
%% otherwise
type1(Gen,TypeName) ->
    Gen(TypeName).


%% repeat
repeat(Gen,Min,Max,_Weight,Element) ->
    ?SIZED(Size,
           begin
               MAX = if Max =/= infinity -> Max; true -> Size end,
               ?LET(K, if Min >= MAX -> Min; true -> choose(Min, MAX) end,
                    vector(K,type(Gen,Element)))
           end).


%% abnf_repeat
abnf_repeat(Gen,Min,Max,Weight,Element) ->
    ?LET(L, repeat(Gen,Min,Max,Weight,Element),
         %% flatten -> append
         abnf_append_byte_seq(lists:flatten(L),<<>>)).


%% abnf_type_byte_seq
abnf_type_byte_seq(_Gen,[],Acc) ->
    abnf_append_byte_seq(lists:reverse(Acc),<<>>);
abnf_type_byte_seq(Gen,[H|T],Acc) ->
    ?LET(Type,type(Gen,H),
         abnf_type_byte_seq(Gen,T,[Type|Acc])).


%% abnf_append_byte_seq
abnf_append_byte_seq([],Acc) ->
    Acc;
abnf_append_byte_seq([H|T],Acc) ->
    abnf_append_byte_seq(T,<<Acc/binary,H/binary>>).

%% abnf_type_seq
abnf_type_seq(_Gen,[],Acc) ->
    %% reverse -> flatten -> append
    abnf_append_byte_seq(lists:flatten(lists:reverse(Acc)),<<>>);
abnf_type_seq(Gen,[H|T],Acc) ->
    ?LET(Type,type(Gen,H),
         abnf_type_seq(Gen,T,[Type|Acc])).

-endif. %% -ifdef(QC).
