Nonterminals
anyrules anyrule annotation
transitions transition outputs types responseAndState
form type typeDef typeRef primType typeAttr typeSeq typeRec.

Terminals
namekwd vsnkwd typekwd statekwd anystatekwd eventkwd atom binary float integer string
 '+' '|'  '=' '#' '{' '}' '&' ';' ',' '[]' '[' ']' '(' ')' ']?' ']{0}' ']{1}' ']+' ')?' '){0}' '){1}' '++' '..' '##' '=>' '<=' dot.


Rootsymbol form.

form -> '+' namekwd '(' string ')' dot      : {name, unwrap('$4')}.
form -> '+' vsnkwd '(' string ')'  dot      : {vsn, unwrap('$4')}.
form -> '+' typekwd types dot               : {types, '$3'}.
form -> '+' statekwd atom transitions dot   : {transition, {unwrap('$3'), '$4'}}.
form -> '+' anystatekwd anyrules dot        : {anystate, '$3'}.

types -> typeDef ';' types                  : ['$1'|'$3'].
types -> typeDef                            : ['$1'].

typeDef -> atom '(' ')' '=' type annotation : {unwrap('$1'), '$5', '$6'}.

annotation -> string                        : unwrap('$1').
annotation -> binary                        : unwrap('$1').
annotation -> '$empty'                      : "".

type ->  primType '|' type                  : eor('$1', '$3').
type ->  primType '++' type                 : econcat('$1', '$3').
type ->  primType                           : '$1'.

primType -> atom '(' ')'                    : {prim, 1, 1, unwrapprim('$1')}.
primType -> atom '(' ')?'                   : {prim, 0, 1, unwrapprim('$1')}.
primType -> atom '(' '){0}'                 : {prim, 0, 0, unwrapprim('$1')}.
primType -> atom '(' '){1}'                 : {prim, 1, 1, unwrapprim('$1')}.
primType -> atom '(' typeAttr ')'           : {prim, 1, 1, unwrapprim('$1', '$3')}.
primType -> atom '(' typeAttr ')?'          : {prim, 0, 1, unwrapprim('$1', '$3')}.
primType -> atom '(' typeAttr '){0}'        : {prim, 0, 0, unwrapprim('$1', '$3')}.
primType -> atom '(' typeAttr '){1}'        : {prim, 1, 1, unwrapprim('$1', '$3')}.

primType -> '{' typeSeq '}'                 : {tuple, '$2'}.

primType -> '#' atom '{' typeRec '}'        : {record, unwrap('$2'),
                                               [unwraprecfields('$4'), add_prim_term()|unwraprecvalues('$4')]}.
primType -> '##' atom '{' typeRec '}'       : {record_ext, unwrap('$2'),
                                               [unwraprecfields('$4'), add_prim_term()|unwraprecvalues('$4')]}.

primType -> '[' type ']'                    : {list, 0, infinity, '$2'}.
primType -> '[' type ']+'                   : {list, 1, infinity, '$2'}.
primType -> '[' type ']?'                   : {list, 0, 1, '$2'}.
primType -> '[' type ']{0}'                 : {list, 0, 0, '$2'}.
primType -> '[' type ']{1}'                 : {list, 1, 1, '$2'}.
primType -> '[' type ']' '{' integer '}'    : {list, '$5', '$5', '$2'}.
primType -> '[' type ']' '{' integer ',' '}': {list, '$5', infinity, '$2'}.
primType -> '[' type ']' '{' integer ',' integer '}'
                                            : {list, '$5', '$6', '$2'}.

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

typeRec -> atom '=' type                    : [{unwrap('$1'),'$3'}].
typeRec -> atom '=' type ',' typeRec        : [{unwrap('$1'),'$3'}|'$5'].

typeRef -> atom '(' ')'                     : {prim, 1, 1, unwrapprim('$1')}.

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

econcat(X, nil) -> X;
econcat(X, Y) -> {concat, X, Y}.

unwrap({V,_}) -> V;
unwrap({_,_,V}) -> V;
unwrap(X) -> erlang:error({invalid,X}).

unwrapprim({V,_}) ->
    case lists:member(V, contract_parser:preDefinedTypesWithoutAttrs()) of
        false ->
            V;
        true ->
            {predef,V}
    end;
unwrapprim({_,_,V}) ->
    case lists:member(V, contract_parser:preDefinedTypesWithoutAttrs()) of
        false ->
            V;
        true ->
            {predef,V}
    end;
unwrapprim(X) -> erlang:error({invalidprim,X}).

%% This is a helper function to add contract checking for predefined
%% types with attributes.
unwrapprim(Prim,L) ->
    {predef,T} = unwrapprim(Prim),
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
            {predef,Type}
    end.

%% This is a helper function to add contract checking for the
%% automatically added '$fields' record and record_ext member.
unwraprecfields(L) ->
    Keys = proplists:get_keys(L),
    if length(Keys) =/= length(L) ->
            erlang:error({invalidrec,L});
       true ->
            noop
    end,
    case Keys of
	[] -> {atom,undefined};
        _ ->
            eor({atom,undefined}, {tuple,[{atom,K} || {K,_} <- L]})
    end.

%% This is a helper function to add contract checking for the values
%% of the record and record_ext member.
unwraprecvalues(L) ->
    [V || {_,V} <- L].

%% This is a helper function to add contract checking for the
%% automatically added '$extra' record and record_ext member.
add_prim_term() ->
    {predef,term}.
