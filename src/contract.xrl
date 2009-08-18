%% Token Definitions for UBF(2)
%% Joe Armstrong (joe@sics.se) 2002-02-22
%% Derived from nex.xrl by Robert Virding

Definitions.
O       = [0-7]
D       = [0-9]
H       = [0-9a-fA-F]
A       = [a-z_A-Z@0-9]
WS      = [\000-\s]    

Rules.
{D}+\.{D}+         :     {token,{float,YYline,list_to_float(YYtext)}}.
{D}+               :     {token,{integer,YYline,list_to_integer(YYtext)}}.
[a-z]{A}*          :     Atom = list_to_atom(YYtext),
                         {token,case reserved_word(Atom) of
                                    true -> {Atom,YYline};
                                    false -> {atom,YYline,Atom}
                                end}.
'[^']+'            :     Atom = list_to_atom(lists:sublist(YYtext, 2, length(YYtext) - 2)),
                         {token,case reserved_word(Atom) of
                                    true -> {Atom,YYline};
                                    false -> {atom,YYline,Atom}
                                end}.
NAME               :     {token,{namekwd,YYline}}.
VSN                :     {token,{vsnkwd, YYline}}.
TYPES              :     {token,{typekwd,YYline}}.
STATE              :     {token,{statekwd,YYline}}.
ANYSTATE           :     {token,{anystatekwd,YYline}}.
EVENT              :     {token,{eventkwd,YYline}}.
<<"[^"]*">>        :     B = list_to_binary(lists:sublist(YYtext, 4, length(YYtext) - 6)),
                         {token,{binary,YYline,B}}.
"[^"]*"            :     S = lists:sublist(YYtext, 2, length(YYtext) - 2),
                         {token,{string,YYline,S}}.
=>                 :     {token,{'=>',YYline}}.
\+\+               :     {token,{'++',YYline}}.
\.\.               :     {token,{'..',YYline}}.
\#\#               :     {token,{'##',YYline}}.
\)\?               :     {token,{')?',YYline}}.
\)\{0\}            :     {token,{'){0}',YYline}}.
\)\{1\}            :     {token,{'){1}',YYline}}.
\]\?               :     {token,{']?',YYline}}.
\]\{0\}            :     {token,{']{0}',YYline}}.
\]\{1\}            :     {token,{']{1}',YYline}}.
\]\+               :     {token,{']+',YYline}}.
\[\]               :     {token,{'[]',YYline}}.
[;&,=+()[\]|<>{}#] :     {token,{list_to_atom(YYtext),YYline}}.
\.{WS}             :     {end_token,{dot,YYline}}.
\.%.*              :     {end_token,{dot,YYline}}. % Must special case this
{WS}+              :     .                         % No token returned,eqivalent
\%.*               :     skip_token.               % to 'skip_token'

Erlang code.

-author('joe@sics.se').
-copyright('Copyright (c) 2001 SICS').

-export([reserved_word/1]).

reserved_word(_) -> false.






