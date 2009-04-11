Nonterminals
anyrules anyrule annotation
transitions transition outputs types responseAndState
form type typeDef typeRef primType typeAttr typeSeq typeRec.

Terminals
event vsn anystate name integer float atom string binary tuple term void typeKwd state
 '+' '|'  '=' '#' '{' '}' '&' ';' ',' '[]' '[' ']' '(' ')' ']?' ']{0}' ']{1}' ']+' ')?' '){0}' '){1}' '++' '..' '##' '=>' dot.


Rootsymbol form.

form -> '+' name '(' string ')' dot         : {name, unwrap('$4')}.
form -> '+' vsn '(' string ')'  dot         : {vsn, unwrap('$4')}.
form -> '+' typeKwd types dot               : {types, '$3'}.
form -> '+' state atom transitions dot      : {transition, {unwrap('$3'), '$4'}}.
form -> '+' anystate anyrules dot           : {anystate, '$3'}.

types -> typeDef ';' types                  : ['$1'|'$3'].
types -> typeDef                            : ['$1'].

typeDef -> atom '(' ')' '=' type annotation : {unwrap('$1'), '$5', '$6'}.

annotation -> string                        : unwrap('$1').
annotation -> binary                        : unwrap('$1').
annotation -> '$empty'                      : "".

type ->  primType '|' type                  : eor('$1', '$3').
type ->  primType '++' type                 : econcat('$1', '$3').
type ->  primType                           : '$1'.

primType -> atom '(' ')?'                   : {prim_optional, unwrap('$1')}.
primType -> atom '(' '){0}'                 : {prim_nil, unwrap('$1')}.
primType -> atom '(' '){1}'                 : {prim_required, unwrap('$1')}.
primType -> atom '(' ')'                    : {prim, unwrap('$1')}.
primType -> atom '(' typeAttr ')?'          : {prim_optional, unwraptypeattr(unwrap('$1'), '$3')}.
primType -> atom '(' typeAttr '){0}'        : {prim_nil, unwraptypeattr(unwrap('$1'), '$3')}.
primType -> atom '(' typeAttr '){1}'        : {prim_required, unwraptypeattr(unwrap('$1'), '$3')}.
primType -> atom '(' typeAttr ')'           : {prim, unwraptypeattr(unwrap('$1'), '$3')}.
primType -> '#' atom '{' typeRec '}'        : {record, unwrap('$2'),
                                               [unwraprecfields('$4'), add_prim_term()|unwraprecvalues('$4')]}.
primType -> '##' atom '{' typeRec '}'       : {record_ext, unwrap('$2'),
                                               [unwraprecfields('$4'), add_prim_term()|unwraprecvalues('$4')]}.
primType -> '{' typeSeq '}'                 : {tuple, '$2'}.
primType -> '[' type ']?'                   : {list_optional, '$2'}.
primType -> '[]'                            : {list_nil, undefined}.
primType -> '[' type ']{0}'                 : {list_nil, '$2'}.
primType -> '[' type ']{1}'                 : {list_required, '$2'}.
primType -> '[' type ']+'                   : {list_required_and_repeatable, '$2'}.
primType -> '[' type ']'                    : {list, '$2'}.
primType -> atom                            : {atom, unwrap('$1')}.
primType -> integer                         : {integer, unwrap('$1')}.
primType -> integer '..' integer            : {range, unwrap('$1'), unwrap('$3')}.
primType -> float                           : {float, unwrap('$1')}.
primType -> string                          : {string, unwrap('$1')}.
primType -> binary                          : {binary, unwrap('$1')}.
primType -> tuple                           : {tuple, unwrap('$1')}.
primType -> term                            : {term, unwrap('$1')}.
primType -> void                            : {void, unwrap('$1')}.

typeAttr -> atom                            : [unwrap('$1')].
typeAttr -> atom ',' typeAttr               : [unwrap('$1')|'$3'].

typeSeq -> type                             : ['$1'].
typeSeq -> type ',' typeSeq                 : ['$1'|'$3'].

typeRec -> atom '=' type                    : [{unwrap('$1'),'$3'}].
typeRec -> atom '=' type ',' typeRec        : [{unwrap('$1'),'$3'}|'$5'].

typeRef -> atom '(' ')'                     : {prim, unwrap('$1')}.

transitions -> transition ';' transitions   : ['$1'|'$3'].
transitions -> transition                   : ['$1'].

transition -> typeRef '=>' outputs          : {input, '$1', '$3'}.
transition -> event   '=>' typeRef          : {event, '$3'}.

outputs -> responseAndState '|' outputs     : ['$1'|'$3'].
outputs -> responseAndState                 : ['$1'].

responseAndState -> typeRef '&' atom        : {output, '$1', unwrap('$3')}.

anyrules -> anyrule ';' anyrules            : ['$1'|'$3'].
anyrules -> anyrule                         : ['$1'].

anyrule -> typeRef '=>' typeRef             : {'$1', '$3'}.

Erlang code.
-export([return_error/2]).

eor(X, nil) -> X;
eor(X, Y) -> {alt, X, Y}.

econcat(X, nil) -> X;
econcat(X, Y) -> {concat, X, Y}.

unwrap({V,_}) -> V;
unwrap({_,_,V}) -> V;
unwrap(X) -> {oops, X}.

%% This is a helper function to add contract checking for types with
%% attributes.
unwraptypeattr(T,L) ->
    UL = lists:usort(L),
    Type = {T,UL},
    FL = [ X || X <- UL, not contracts:isTypeAttr(T,X) ],
    if FL =/= [] ->
            erlang:error({invalidtypeattr,{T,FL}});
       true ->
            noop
    end,
    case lists:member(Type, contract_parser:preDefinedTypesWithAttrs()) of
        false ->
            erlang:error({invalidtype,Type});
        true ->
            noop
    end,
    Type.

%% This is a helper function to add contract checking for the
%% the automatically added '$fields' record and record_ext member.
unwraprecfields(L) ->
    Keys = proplists:get_keys(L),
    if length(Keys) =/= length(L) ->
            erlang:error({invalidrec,L});
       true ->
            noop
    end,
    if length(Keys) =:= 0 ->
            {atom,undefined};
       true ->
            eor({atom,undefined}, {tuple,[{atom,K} || {K,_} <- L]})
    end.

unwraprecvalues(L) ->
    [V || {_,V} <- L].

%% This is a helper function to add contract checking for the
%% the automatically added '$extra' record and record_ext member.
add_prim_term() ->
    {prim,term}.
