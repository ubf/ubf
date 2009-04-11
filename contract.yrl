Nonterminals
anyrules anyrule annotation
transitions transition outputs types responseAndState strings
form type typeDef typeRef primType typeSeq.

Terminals
event vsn atom anystate
name integer string typeKwd state
 '+' '|'  '=' '{' '}' '&' ';' ',' '.' '[' ']' '(' ')' '=>' dot.


Rootsymbol form.

form -> '+' name '(' string ')' dot    : {name, unwrap('$4')}.
form -> '+' vsn '(' string ')'  dot    : {vsn, unwrap('$4')}.
form -> '+' typeKwd types dot          : {types, '$3'}.
form -> '+' state atom transitions dot : {transition, {unwrap('$3'), '$4'}}. 
form -> '+' anystate anyrules dot      : {anystate, '$3'}.

types -> typeDef ';' types		: ['$1'|'$3'].
types -> typeDef                        : ['$1'].

typeDef -> atom '(' ')' '=' type annotation:  {unwrap('$1'), '$5', '$6'}.

annotation -> string			: unwrap('$1').
annotation -> '$empty'                 : "".

type ->  primType '|' type	        : eor('$1', '$3').
type ->  primType			: '$1'.

primType -> atom '(' ')'   		: {prim, unwrap('$1')}.
primType -> '{' typeSeq '}'		: {tuple, '$2'}.
primType -> '[' type ']' 		: {list, '$2'}.
primType -> atom	     		: {constant, unwrap('$1')}.
primType -> integer 			: {integer, unwrap('$1')}.
primType -> integer '.' '.' integer	: {range, unwrap('$1'), unwrap('$4')}.
primType -> string 			: {string, unwrap('$1')}.

typeSeq -> type				: ['$1'].
typeSeq -> type ',' typeSeq 		: ['$1'|'$3'].

typeRef -> atom '(' ')'                 : {prim, unwrap('$1')}.

transitions -> transition ';' transitions: ['$1'|'$3'].
transitions -> transition                : ['$1'].

transition -> typeRef '=>' outputs       : {input, '$1', '$3'}.
transition -> event   '=>' typeRef       : {event, '$3'}. 

outputs -> responseAndState '|' outputs: ['$1'|'$3'].
outputs -> responseAndState            : ['$1'].

responseAndState -> typeRef '&' atom   :  {output, '$1', unwrap('$3')}.

anyrules -> anyrule ';' anyrules  : ['$1'|'$3'].
anyrules -> anyrule		  : ['$1'].

anyrule -> typeRef '=>' typeRef	  : {'$1', '$3'}.

strings -> string ',' strings : [unwrap('$1')|'$3'].
strings -> string             : [unwrap('$1')].
strings -> '$empty'           : [].

Erlang code.
-export([return_error/2]).

eor(X, nil) -> X;
eor(X, Y) -> {alt, X, Y}.

unwrap({V,_}) -> V;	
unwrap({_,_,V}) -> V;
unwrap(X) -> {oops, X}.

