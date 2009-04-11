Nonterminals
transitions transition outputs typeAndState strings
form type orType annotatedType primType annotation typeSeq.

Terminals
info description  event vsn atom services  
name integer string typeKwd state
':' '+' '|'  '=' '{' '}' '&' ';' ',' '.' '[' ']' '(' ')' '=>' dot.


Rootsymbol form.

form ->  '+' name '(' string ')' dot: {name, unwrap('$4')}.
form ->  '+' vsn '(' string ')' dot : {vsn, unwrap('$4')}.
form ->  '+' typeKwd atom '(' ')' '=' type dot : {type, {unwrap('$3'), '$7'}}.
form ->  '+' state atom transitions dot: {transition, {unwrap('$3'), '$4'}}. 
form ->  '+' info '(' string ')' dot: {info, unwrap('$4')}.
form ->  '+' description '(' string ')' dot: {description, unwrap('$4')}.
form ->  '+' services '(' strings ')' dot: {services, '$4'}.

type -> annotatedType orType            : eor('$1', '$2').

orType -> '|' annotatedType orType	: eor('$2', '$3').
orType -> '$empty'			: nil.

annotatedType -> atom ':' ':' primType	: '$4'.
annotatedType -> primType               : '$1'.

annotation -> atom                      : unwrap('$1').

primType -> atom '(' ')'   		: {prim, unwrap('$1')}.
primType -> '{' typeSeq '}'		: {tuple, '$2'}.
primType -> '[' type ']' 		: {list, '$2'}.
primType -> atom	     		: {constant, unwrap('$1')}.
primType -> integer 			: {integer, unwrap('$1')}.
primType -> integer '.' '.' integer	: {range, unwrap('$1'), unwrap('$4')}.
primType -> string 			: {string, unwrap('$1')}.

typeSeq -> type				: ['$1'].
typeSeq -> type ',' typeSeq 		: ['$1'|'$3'].

transitions -> transition ';' transitions: ['$1'|'$3'].
transitions -> transition                 : ['$1'].

transition -> type '=>' outputs: {input, '$1', '$3'}.
transition -> event '=>' type    : {event, '$3'}. 

outputs -> typeAndState '|' outputs: ['$1'|'$3'].
outputs -> typeAndState            : ['$1'].

typeAndState -> type '&' atom: {output, '$1', unwrap('$3')}.

strings -> string ',' strings : [unwrap('$1')|'$3'].
strings -> string             : [unwrap('$1')].
strings -> '$empty'           : [].

Erlang code.

eor(X, nil) -> X;
eor(X, Y) -> {alt, X, Y}.

unwrap({V,_}) -> V;	
unwrap({_,_,V}) -> V.

simplify({Tag,A,nil}) -> A;
simplify(X) -> X.
