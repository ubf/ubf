%%% -*- mode: erlang -*-
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

Nonterminals
anyrules anyrule annotation
transitions transition outputs types responseAndState
form type typeDef typeRef primType typeAttr typeSeq typeRec default.

Terminals
namekwd vsnkwd typekwd statekwd anystatekwd eventkwd atom binary float integer string tag
'+' '|'  '=' '#' '{}' '{' '}' '&' ';' ',' '[]' '[' ']' '(' ')' ']?' ']+' ')?' '..' '##' '=>' '<=' '::' dot.

Rootsymbol form.

form -> '+' namekwd '(' string ')' dot      : {name, unwrap('$4')}.
form -> '+' vsnkwd '(' string ')' dot       : {vsn, unwrap('$4')}.
form -> '+' typekwd types dot               : {types, '$3'}.
form -> '+' statekwd atom transitions dot   : {transition, {unwrap('$3'), '$4'}}.
form -> '+' anystatekwd anyrules dot        : {anystate, '$3'}.

types -> typeDef ';' types                  : ['$1'|'$3'].
types -> typeDef                            : ['$1'].

typeDef -> atom '(' ')' '::' type annotation : {unwrap('$1'), '$5', '$6'}.

annotation -> tag                           : unwrap('$1').
annotation -> string                        : unwrap('$1').
annotation -> binary                        : unwrap('$1').
annotation -> '$empty'                      : "".

type ->  primType '|' type                  : eor('$1', '$3').
type ->  primType                           : '$1'.

primType -> atom '(' ')'                    : {prim, 1, 1, unwrap_prim('$1')}.
primType -> atom '(' ')?'                   : {prim, 0, 1, unwrap_prim('$1')}.
primType -> atom '(' typeAttr ')'           : {prim, 1, 1, unwrap_prim('$1', '$3')}.
primType -> atom '(' typeAttr ')?'          : {prim, 0, 1, unwrap_prim('$1', '$3')}.

primType -> '{' typeSeq '}'                 : {tuple, list_to_tuple('$2')}.

primType -> '#' atom '{' typeRec '}'        : rec(unwrap('$2'), '$4').
primType -> '##' atom '{' typeRec '}'       : rec_ext(unwrap('$2'), '$4').

primType -> '[' type ']'                    : {list, 0, infinity, '$2'}.
primType -> '[' type ']+'                   : {list, 1, infinity, '$2'}.
primType -> '[' type ']?'                   : {list, 0, 1, '$2'}.
primType -> '[' type ']' '{' integer '}'    : {list, unwrap('$5'), unwrap('$5'), '$2'}.
primType -> '[' type ']' '{' integer ',' '}': {list, unwrap('$5'), infinity, '$2'}.
primType -> '[' type ']' '{' ',' integer '}': {list, 0, unwrap('$6'), '$2'}.
primType -> '[' type ']' '{' integer ',' integer '}' : {list, unwrap('$5'), unwrap('$7'), '$2'}.

primType -> '{}'                            : {tuple, {}}.
primType -> '#' atom '{}'                   : rec(unwrap('$2'), []).
primType -> '##' atom '{}'                  : rec_ext(unwrap('$2'), []).

primType -> '[]'                            : {list, 0, 0, undefined}.

primType -> integer '..' integer            : {range, unwrap('$1'), unwrap('$3')}.
primType -> '..' integer                    : {range, infinity, unwrap('$2')}.
primType -> integer '..'                    : {range, unwrap('$1'), infinity}.

primType -> atom                            : {atom, unwrap('$1')}.
primType -> binary                          : {binary, unwrap('$1')}.
primType -> float                           : {float, unwrap('$1')}.
primType -> integer                         : {integer, unwrap('$1')}.
primType -> string                          : {string, unwrap('$1')}.

typeAttr -> atom                            : [unwrap('$1')].
typeAttr -> atom ',' typeAttr               : [unwrap('$1')|'$3'].

typeSeq -> type                             : ['$1'].
typeSeq -> type ',' typeSeq                 : ['$1'|'$3'].

typeRec -> atom '::' type                   : [{unwrap('$1'),[],'$3'}].
typeRec -> atom '::' type ',' typeRec       : [{unwrap('$1'),[],'$3'}|'$5'].
typeRec -> atom '=' default '::' type       : [{unwrap('$1'),['$3'],'$5'}].
typeRec -> atom '=' default '::' type ',' typeRec : [{unwrap('$1'),['$3'],'$5'}|'$7'].

typeRef -> atom '(' ')'                     : {prim, 1, 1, unwrap_prim('$1')}.

default -> type                             : '$1'.

transitions -> transition ';' transitions   : ['$1'|'$3'].
transitions -> transition                   : ['$1'].

transition -> typeRef '=>' outputs          : {input, '$1', '$3'}.
transition -> eventkwd '=>' typeRef         : {event_out, '$3'}.
transition -> eventkwd '<=' typeRef         : {event_in, '$3'}.

outputs -> responseAndState '|' outputs     : ['$1'|'$3'].
outputs -> responseAndState                 : ['$1'].

responseAndState -> typeRef '&' atom        : {output, '$1', unwrap('$3')}.

anyrules -> anyrule ';' anyrules            : ['$1'|'$3'].
anyrules -> anyrule                         : ['$1'].

anyrule -> typeRef '=>' typeRef             : {'$1', '$3'}.
anyrule -> eventkwd '=>' typeRef            : {event_out, '$3'}.
anyrule -> eventkwd '<=' typeRef            : {event_in, '$3'}.

Erlang code.

eor(X, nil) -> X;
eor(X, Y) -> {alt, X, Y}.

unwrap({V,_}) -> V;
unwrap({_,_,V}) -> V;
unwrap(X) -> erlang:error({invalid,X}).

unwrap_prim({V,_}) ->
    case lists:member(V, contract_parser:predefined_types(withoutattrs)) of
        false ->
            V;
        true ->
            {predef,V}
    end;
unwrap_prim({_,_,V}) ->
    case lists:member(V, contract_parser:predefined_types(withoutattrs)) of
        false ->
            V;
        true ->
            {predef,V}
    end;
unwrap_prim(X) -> erlang:error({invalidprim,X}).

%% This is a helper function to add contract checking for predefined
%% types with attributes.
unwrap_prim(Prim,L) ->
    {predef,T} = unwrap_prim(Prim),
    UL = lists:usort(L),
    Type = {T,UL},
    FL = [ X || X <- UL, not contracts:isTypeAttr(T,X) ],
    if FL =/= [] ->
            erlang:error({invalidtypeattr,{T,FL}});
       true ->
            noop
    end,
    case lists:member(Type, contract_parser:predefined_types(withattrs)) of
        false ->
            erlang:error({invalidtype,Type});
        true ->
            {predef,Type}
    end.

rec(Name, Args) ->
    Fields = rec_fields(Args),
    Defaults = rec_defaults(Args),
    Types = list_to_tuple([{atom,Name}|rec_types(Args)]),
    {record, Name, Fields, Defaults, Types}.

rec_ext(Name, Args) ->
    Fields = ['$fields','$extra'|rec_fields(Args)],
    Defaults = [[Fields],[]|rec_defaults(Args)],
    Types = rec_types(Args),
    Types1 = list_to_tuple(
               if Types == [] ->
                       [{atom,Name},
                        {atom,undefined},
                        {predef,any}];
                  true ->
                       [{atom,Name},
                        eor({atom,undefined}, {tuple, [ {atom,X} || X <- Fields ]}),
                        {predef,any}|Types]
               end),
    {record_ext, Name, Fields, Defaults, Types1}.

rec_fields(L) ->
    L1 = [ rec_field(X) || X <- L ],
    L2 = lists:sort(L1),
    L3 = lists:usort(L1),
    if L2 =/= L3 ->
            erlang:error({invalidrec,L});
       true ->
            noop
    end,
    L1.

rec_defaults(L) ->
    [ rec_default(X) || X <- L ].

rec_types(L) ->
    [ rec_type(X) || X <- L ].

rec_field({X,_Y,_Z}) ->
    X.

rec_default({_X,Y,_Z}) ->
    Y.

rec_type({_X,_Y,Z}) ->
    Z.
