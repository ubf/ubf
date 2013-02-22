%%% -*- mode: erlang -*-
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

%%% Token Definitions for UBF(2)
%%% Joe Armstrong (joe@sics.se) 2002-02-22
%%% Derived from nex.xrl by Robert Virding

Definitions.
O       = [0-7]
D       = [0-9]
H       = [0-9a-fA-F]
A       = [a-z_A-Z@0-9]
WS      = [\000-\s]

Rules.
\-?{D}+\.{D}+      :     {token,{float,TokenLine,list_to_float(TokenChars)}}.
\-?{D}+            :     {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{D}+\#{H}+         :     {token,{integer,TokenLine,parse_erlang_single_expr(TokenChars)}}.
[a-z]{A}*          :     Atom = list_to_atom(TokenChars),
                         {token,case reserved_word(Atom) of
                                    %% silence dialyzer warnings
                                    %% true -> {Atom,TokenLine};
                                    false -> {atom,TokenLine,Atom}
                                end}.
'[^']*'            :     Atom = list_to_atom(lists:sublist(TokenChars, 2, length(TokenChars) - 2)),
                         {token,case reserved_word(Atom) of
                                    %% silence dialyzer warnings
                                    %% true -> {Atom,TokenLine};
                                    false -> {atom,TokenLine,Atom}
                                end}.
NAME               :     {token,{namekwd,TokenLine}}.
VSN                :     {token,{vsnkwd, TokenLine}}.
TYPES              :     {token,{typekwd,TokenLine}}.
STATE              :     {token,{statekwd,TokenLine}}.
ANYSTATE           :     {token,{anystatekwd,TokenLine}}.
EVENT              :     {token,{eventkwd,TokenLine}}.
<<"[^"]*">>        :     B = list_to_binary(lists:sublist(TokenChars, 4, length(TokenChars) - 6)),
                         {token,{binary,TokenLine,B}}.
"[^"]*"            :     S = lists:sublist(TokenChars, 2, length(TokenChars) - 2),
                         {token,{string,TokenLine,S}}.
`[^`]*`            :     S = lists:sublist(TokenChars, 2, length(TokenChars) - 2),
                         {token,{tag,TokenLine,S}}.
=>                 :     {token,{'=>',TokenLine}}.
<=                 :     {token,{'<=',TokenLine}}.
\.\.               :     {token,{'..',TokenLine}}.
\#\#               :     {token,{'##',TokenLine}}.
\)\?               :     {token,{')?',TokenLine}}.
\]\?               :     {token,{']?',TokenLine}}.
\]\+               :     {token,{']+',TokenLine}}.
\{\}               :     {token,{'{}',TokenLine}}.
\[\]               :     {token,{'[]',TokenLine}}.
\:\:               :     {token,{'::',TokenLine}}.
[;&,=+()[\]|<>{}#] :     {token,{list_to_atom(TokenChars),TokenLine}}.
\.{WS}             :     {end_token,{dot,TokenLine}}.
\.%.*              :     {end_token,{dot,TokenLine}}. % Must special case this
{WS}+              :     .                            % No token returned,equivalent
\%.*               :     skip_token.                  % to 'skip_token'


Erlang code.

-author('joe@sics.se').
-copyright('Copyright (c) 2001 SICS').

-export([reserved_word/1, parse_erlang_single_expr/1]).

reserved_word(_) -> false.

parse_erlang_single_expr(Str0) ->
    Str = case lists:last(Str0) of $. -> Str0; _  -> Str0 ++ "." end,
    {ok, Ts, _} = erl_scan:string(Str),
    {ok, Rs} = erl_parse:parse_exprs(Ts),
    {value, Val, _} = erl_eval:exprs(Rs, []),
    Val.
