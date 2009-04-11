%% THIS IS A PRE-RELEASE OF LEEX - RELEASED ONLY BECAUSE MANY PEOPLE
%% WANTED IT - THE OFFICIAL RELEASE WILL PROVIDE A DIFFERENT INCOMPATIBLE
%% AND BETTER INTERFACE - BE WARNED
%% PLEASE REPORT ALL BUGS TO THE AUTHOR.

-module('contract_lex').

-export([string/1,string/2,token/2,token/3,tokens/2,tokens/3]).
-export([format_error/1]).

%% User code. This is placed here to allow extra attributes.

-author('joe@sics.se').
-copyright('Copyright (c) 2001 SICS').

-export([reserved_word/1]).

reserved_word(_) -> false.







format_error({illegal,S}) -> ["illegal characters ",io_lib:write_string(S)];
format_error({user,S}) -> S.

string(String) -> string(String, 1).

string(String, Line) -> string(String, Line, String, []).

%% string(InChars, Line, TokenChars, Tokens) ->
%%    {ok,Tokens,Line} | {error,ErrorInfo,Line}.

string([], L, [], Ts) ->			%No partial tokens!
    {ok,yyrev(Ts),L};
string(Ics0, L0, Tcs, Ts) ->
    case yystate(yystate(), Ics0, L0, 0, reject, 0) of
	{A,Alen,Ics1,L1} ->			%Accepting end state
	    string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L1), Ts);
	{A,Alen,Ics1,L1,S1} ->		%After an accepting state
	    string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L1), Ts);
	{reject,Alen,Tlen,Ics1,L1,S1} ->
	    {error,{L1,?MODULE,{illegal,yypre(Tcs, Tlen+1)}},L1};
	{A,Alen,Tlen,Ics1,L1,S1} ->
	    string_cont(yysuf(Tcs, Alen), L1, yyaction(A, Alen, Tcs, L1), Ts)
    end.

%% string_cont(RestChars, Line, Token, Tokens)
%%  Test for and remove the end token wrapper.

string_cont(Rest, Line, {token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {end_token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, skip_token, Ts) ->
    string(Rest, Line, Rest, Ts);
string_cont(Rest, Line, {error,S}, Ts) ->
    {error,{Line,?MODULE,{user,S}},Line}.

%% token(Continuation, Chars, Line) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept.

token(Cont, Chars) -> token(Cont, Chars, 1).

token([], Chars, Line) ->
    token(Chars, Line, yystate(), Chars, 0, reject, 0);
token({Line,State,Tcs,Tlen,Action,Alen}, Chars, _) ->
    token(Chars, Line, State, Tcs ++ Chars, Tlen, Action, Alen).

%% token(InChars, Line, State, TokenChars, TokenLen, Accept) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

token(Ics0, L0, S0, Tcs, Tlen0, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1));
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{L1,S1,Tcs,Alen1,A1,Alen1}};
	{A1,Alen1,Ics1,L1,S1} ->
	    token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1));
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{L1,S1,Tcs,Tlen1,A1,Alen1}};
	{reject,Alen1,Tlen1,eof,L1,S1} ->
	    {done,{eof,L1},[]};
	{reject,Alen1,Tlen1,Ics1,L1,S1} ->
	    {done,{error,{L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},L1},Ics1};
	{A1,Alen1,Tlen1,Ics1,L1,S1} ->
	    token_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1))
    end.

%% tokens_cont(RestChars, Line, Token)
%%  Test if we have detected the end token, if so return done else continue.

token_cont(Rest, Line, {token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {end_token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, skip_token) ->
    token(Rest, Line, yystate(), Rest, 0, reject, 0);
token_cont(Rest, Line, {error,S}) ->
    {done,{error,{Line,?MODULE,{user,S}},Line},Rest}.

%% tokens(Continuation, Chars, Line) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept.

tokens(Cont, Chars) -> tokens(Cont, Chars, 1).

tokens([], Chars, Line) ->
    tokens(Chars, Line, yystate(), Chars, 0, [], reject, 0);
tokens({tokens,Line,State,Tcs,Tlen,Ts,Action,Alen}, Chars, _) ->
    tokens(Chars, Line, State, Tcs ++ Chars, Tlen, Ts, Action, Alen);
tokens({skip_tokens,Line,State,Tcs,Tlen,Error,Action,Alen}, Chars, _) ->
    skip_tokens(Chars, Line, State, Tcs ++ Chars, Tlen, Error, Action, Alen).

%% tokens(InChars, Line, State, TokenChars, TokenLen, Tokens, Accept) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

tokens(Ics0, L0, S0, Tcs, Tlen0, Ts, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Ts);
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{tokens,L1,S1,Tcs,Alen1,Ts,A1,Alen1}};
	{A1,Alen1,Ics1,L1,S1} ->
	    tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Ts);
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{tokens,L1,S1,Tcs,Tlen1,Ts,A1,Alen1}};
	{reject,Alen1,Tlen1,eof,L1,S1} ->
	    {done,if Ts == [] -> {eof,L1};
		     true -> {ok,yyrev(Ts),L1} end,[]};
	{reject,Alen1,Tlen1,Ics1,L1,S1} ->
	    skip_tokens(yysuf(Tcs, Tlen1+1), L1,
			{L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}});
	{A1,Alen1,Tlen1,Ics1,L1,S1} ->
	    tokens_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1), Ts)
    end.

%% tokens_cont(RestChars, Line, Token, Tokens)
%%  Test if we have detected the end token, if so return done else continue.

tokens_cont(Rest, Line, {token,T}, Ts) ->
    tokens(Rest, Line, yystate(), Rest, 0, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {end_token,T}, Ts) ->
    {done,{ok,yyrev(Ts, [T]),Line},Rest};
tokens_cont(Rest, Line, skip_token, Ts) ->
    tokens(Rest, Line, yystate(), Rest, 0, Ts, reject, 0);
tokens_cont(Rest, Line, {error,S}, Ts) ->
    skip_tokens(Rest, Line, {Line,?MODULE,{user,S}}).

%% token_skip(InChars, Line, Error) -> {done,ReturnVal,RestChars}.
%%  Skip tokens until an end token, junk everything and return the error.

%%skip_tokens(Ics, Line, Error) -> {done,{error,Error,Line},Ics}.

skip_tokens(Ics, Line, Error) ->
    skip_tokens(Ics, Line, yystate(), Ics, 0, Error, reject, 0).

%% skip_tokens(InChars, Line, State, TokenChars, TokenLen, Tokens, Accept) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

skip_tokens(Ics0, L0, S0, Tcs, Tlen0, Error, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Error);
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{skip_tokens,L1,S1,Tcs,Alen1,Error,A1,Alen1}};
	{A1,Alen1,Ics1,L1,S1} ->
	    skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Error);
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{skip_tokens,L1,S1,Tcs,Tlen1,Error,A1,Alen1}};
	{reject,Alen1,Tlen1,eof,L1,S1} ->
	    {done,{error,Error,L1},[]};
	{reject,Alen1,Tlen1,Ics1,L1,S1} ->
	    skip_tokens(yysuf(Tcs, Tlen1), L1, Error);
	{A1,Alen1,Tlen1,Ics1,L1,S1} ->
	    skip_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1), Error)
    end.

%% skip_cont(RestChars, Line, Token, Error)
%%  Test if we have detected the end token, if so return done else continue.

skip_cont(Rest, Line, {token,T}, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0);
skip_cont(Rest, Line, {end_token,T}, Error) ->
    {done,{error,Error,Line},Rest};
skip_cont(Rest, Line, {error,S}, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0);
skip_cont(Rest, Line, skip_token, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0).

yyrev(L) -> yyrev(L, []).

yyrev([H|T], Acc) -> yyrev(T, [H|Acc]);
yyrev([], Acc) -> Acc.

yypre([H|T], N) when N > 0 -> [H|yypre(T, N-1)];
yypre(L, N) -> [].

yysuf([H|T], N) when N > 0 -> yysuf(T, N-1);
yysuf(L, 0) -> L.

%% yystate() -> InitialState.
%% yystate(State, InChars, Line, Token,  ) ->
%%      {Action, AcceptLength, RestChars, Line} |         Accepting end state
%%      {Action, AcceptLength, RestChars, Line, State} |  Accepting state
%%      {Action, AcceptLength, TokLength, RestChars, Line, State} |
%%      {reject, AcceptLength, TokLength, RestChars, Line, State}.
%% Generated state transition functions.

yystate() -> 53.

yystate(56, [$F|Ics], Line, Tlen, Action, Alen) ->
    yystate(52, Ics, Line, Tlen+1, 17, Tlen);
yystate(56, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $E ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(56, [C|Ics], Line, Tlen, Action, Alen) when C >= $G, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(56, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,56};
yystate(55, Ics, Line, Tlen, Action, Alen) ->
    {12,Tlen,Ics,Line};
yystate(54, [$N|Ics], Line, Tlen, Action, Alen) ->
    yystate(56, Ics, Line, Tlen+1, 17, Tlen);
yystate(54, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $M ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(54, [C|Ics], Line, Tlen, Action, Alen) when C >= $O, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(54, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,54};
yystate(53, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(49, Ics, Line+1, Tlen+1, Action, Alen);
yystate(53, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(45, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$%|Ics], Line, Tlen, Action, Alen) ->
    yystate(37, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$&|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$(|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$)|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$+|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$,|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$;|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$<|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$>|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$D|Ics], Line, Tlen, Action, Alen) ->
    yystate(9, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$I|Ics], Line, Tlen, Action, Alen) ->
    yystate(54, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$N|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$S|Ics], Line, Tlen, Action, Alen) ->
    yystate(28, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$T|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$U|Ics], Line, Tlen, Action, Alen) ->
    yystate(47, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$V|Ics], Line, Tlen, Action, Alen) ->
    yystate(35, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$[|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(49, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $\s ->
    yystate(49, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $C ->
    yystate(47, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [C|Ics], Line, Tlen, Action, Alen) when C >= $F, C =< $H ->
    yystate(47, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [C|Ics], Line, Tlen, Action, Alen) when C >= $J, C =< $M ->
    yystate(47, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [C|Ics], Line, Tlen, Action, Alen) when C >= $O, C =< $R ->
    yystate(47, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [C|Ics], Line, Tlen, Action, Alen) when C >= $W, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(53, [C|Ics], Line, Tlen, Action, Alen) when C >= ${, C =< $} ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(53, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,53};
yystate(52, [$O|Ics], Line, Tlen, Action, Alen) ->
    yystate(48, Ics, Line, Tlen+1, 17, Tlen);
yystate(52, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $N ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(52, [C|Ics], Line, Tlen, Action, Alen) when C >= $P, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(52, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,52};
yystate(51, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(51, Ics, Line, Tlen+1, 1, Tlen);
yystate(51, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(51, Ics, Line, Tlen+1, 1, Tlen);
yystate(51, [C|Ics], Line, Tlen, Action, Alen) when C >= $@, C =< $Z ->
    yystate(51, Ics, Line, Tlen+1, 1, Tlen);
yystate(51, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(51, Ics, Line, Tlen+1, 1, Tlen);
yystate(51, Ics, Line, Tlen, Action, Alen) ->
    {1,Tlen,Ics,Line,51};
yystate(50, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 5, Tlen);
yystate(50, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,50};
yystate(49, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(49, Ics, Line+1, Tlen+1, 15, Tlen);
yystate(49, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(49, Ics, Line, Tlen+1, 15, Tlen);
yystate(49, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $\s ->
    yystate(49, Ics, Line, Tlen+1, 15, Tlen);
yystate(49, Ics, Line, Tlen, Action, Alen) ->
    {15,Tlen,Ics,Line,49};
yystate(48, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 7, Tlen);
yystate(48, Ics, Line, Tlen, Action, Alen) ->
    {7,Tlen,Ics,Line,48};
yystate(47, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(47, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,47};
yystate(46, [$T|Ics], Line, Tlen, Action, Alen) ->
    yystate(50, Ics, Line, Tlen+1, 17, Tlen);
yystate(46, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $S ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(46, [C|Ics], Line, Tlen, Action, Alen) when C >= $U, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(46, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,46};
yystate(45, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(45, Ics, Line+1, Tlen+1, Action, Alen);
yystate(45, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(45, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(45, Ics, Line, Tlen+1, Action, Alen);
yystate(45, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(45, Ics, Line, Tlen+1, Action, Alen);
yystate(45, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $ÿ ->
    yystate(45, Ics, Line, Tlen+1, Action, Alen);
yystate(45, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,45};
yystate(44, [$A|Ics], Line, Tlen, Action, Alen) ->
    yystate(40, Ics, Line, Tlen+1, 17, Tlen);
yystate(44, [C|Ics], Line, Tlen, Action, Alen) when C >= $B, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(44, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,44};
yystate(43, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 2, Tlen);
yystate(43, Ics, Line, Tlen, Action, Alen) ->
    {2,Tlen,Ics,Line,43};
yystate(42, [$N|Ics], Line, Tlen, Action, Alen) ->
    yystate(46, Ics, Line, Tlen+1, 17, Tlen);
yystate(42, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $M ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(42, [C|Ics], Line, Tlen, Action, Alen) when C >= $O, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(42, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,42};
yystate(41, Ics, Line, Tlen, Action, Alen) ->
    {10,Tlen,Ics,Line};
yystate(40, [$M|Ics], Line, Tlen, Action, Alen) ->
    yystate(36, Ics, Line, Tlen+1, 17, Tlen);
yystate(40, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $L ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(40, [C|Ics], Line, Tlen, Action, Alen) when C >= $N, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(40, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,40};
yystate(39, [$N|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 17, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $M ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $O, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(39, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,39};
yystate(38, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(42, Ics, Line, Tlen+1, 17, Tlen);
yystate(38, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $D ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(38, [C|Ics], Line, Tlen, Action, Alen) when C >= $F, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(38, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,38};
yystate(37, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(37, Ics, Line, Tlen+1, 16, Tlen);
yystate(37, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $ÿ ->
    yystate(37, Ics, Line, Tlen+1, 16, Tlen);
yystate(37, Ics, Line, Tlen, Action, Alen) ->
    {16,Tlen,Ics,Line,37};
yystate(36, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(32, Ics, Line, Tlen+1, 17, Tlen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $D ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= $F, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(36, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,36};
yystate(35, [$S|Ics], Line, Tlen, Action, Alen) ->
    yystate(39, Ics, Line, Tlen+1, 17, Tlen);
yystate(35, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $R ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(35, [C|Ics], Line, Tlen, Action, Alen) when C >= $T, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(35, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,35};
yystate(34, [$V|Ics], Line, Tlen, Action, Alen) ->
    yystate(38, Ics, Line, Tlen+1, 17, Tlen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $U ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= $W, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(34, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,34};
yystate(33, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(29, Ics, Line+1, Tlen+1, Action, Alen);
yystate(33, [$%|Ics], Line, Tlen, Action, Alen) ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(29, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $\s ->
    yystate(29, Ics, Line, Tlen+1, Action, Alen);
yystate(33, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,33};
yystate(32, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 6, Tlen);
yystate(32, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,32};
yystate(31, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 3, Tlen);
yystate(31, Ics, Line, Tlen, Action, Alen) ->
    {3,Tlen,Ics,Line,31};
yystate(30, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 8, Tlen);
yystate(30, Ics, Line, Tlen, Action, Alen) ->
    {8,Tlen,Ics,Line,30};
yystate(29, Ics, Line, Tlen, Action, Alen) ->
    {13,Tlen,Ics,Line};
yystate(28, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line, Tlen+1, 17, Tlen);
yystate(28, [$T|Ics], Line, Tlen, Action, Alen) ->
    yystate(3, Ics, Line, Tlen+1, 17, Tlen);
yystate(28, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $D ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(28, [C|Ics], Line, Tlen, Action, Alen) when C >= $F, C =< $S ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(28, [C|Ics], Line, Tlen, Action, Alen) when C >= $U, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(28, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,28};
yystate(27, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(31, Ics, Line, Tlen+1, 17, Tlen);
yystate(27, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $D ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(27, [C|Ics], Line, Tlen, Action, Alen) when C >= $F, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(27, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,27};
yystate(26, [$N|Ics], Line, Tlen, Action, Alen) ->
    yystate(30, Ics, Line, Tlen+1, 17, Tlen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $M ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= $O, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(26, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,26};
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(25, Ics, Line, Tlen+1, 14, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $ÿ ->
    yystate(25, Ics, Line, Tlen+1, 14, Tlen);
yystate(25, Ics, Line, Tlen, Action, Alen) ->
    {14,Tlen,Ics,Line,25};
yystate(24, [$R|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, 17, Tlen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Q ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= $S, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(24, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,24};
yystate(23, [$P|Ics], Line, Tlen, Action, Alen) ->
    yystate(27, Ics, Line, Tlen+1, 17, Tlen);
yystate(23, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $O ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(23, [C|Ics], Line, Tlen, Action, Alen) when C >= $Q, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(23, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,23};
yystate(22, [$O|Ics], Line, Tlen, Action, Alen) ->
    yystate(26, Ics, Line, Tlen+1, 17, Tlen);
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $N ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= $P, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(22, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,22};
yystate(21, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(21, Ics, Line, Tlen+1, 0, Tlen);
yystate(21, Ics, Line, Tlen, Action, Alen) ->
    {0,Tlen,Ics,Line,21};
yystate(20, [$V|Ics], Line, Tlen, Action, Alen) ->
    yystate(16, Ics, Line, Tlen+1, 17, Tlen);
yystate(20, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $U ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(20, [C|Ics], Line, Tlen, Action, Alen) when C >= $W, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(20, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,20};
yystate(19, [$Y|Ics], Line, Tlen, Action, Alen) ->
    yystate(23, Ics, Line, Tlen+1, 17, Tlen);
yystate(19, [$Z|Ics], Line, Tlen, Action, Alen) ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(19, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $X ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(19, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,19};
yystate(18, [$I|Ics], Line, Tlen, Action, Alen) ->
    yystate(22, Ics, Line, Tlen+1, 17, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $H ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $J, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(18, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,18};
yystate(17, [$>|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line, Tlen+1, 12, Tlen);
yystate(17, Ics, Line, Tlen, Action, Alen) ->
    {12,Tlen,Ics,Line,17};
yystate(16, [$I|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 17, Tlen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $H ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $J, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(16, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,16};
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 4, Tlen);
yystate(15, Ics, Line, Tlen, Action, Alen) ->
    {4,Tlen,Ics,Line,15};
yystate(14, [$T|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, 17, Tlen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $S ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $U, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(14, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,14};
yystate(13, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line};
yystate(12, [$A|Ics], Line, Tlen, Action, Alen) ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(12, [$B|Ics], Line, Tlen, Action, Alen) ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(12, [$C|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line, Tlen+1, 17, Tlen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= $D, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(12, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,12};
yystate(11, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, 17, Tlen);
yystate(11, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $D ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(11, [C|Ics], Line, Tlen, Action, Alen) when C >= $F, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(11, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,11};
yystate(10, [$P|Ics], Line, Tlen, Action, Alen) ->
    yystate(14, Ics, Line, Tlen+1, 17, Tlen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $O ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= $Q, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(10, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,10};
yystate(9, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(5, Ics, Line, Tlen+1, 17, Tlen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $D ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= $F, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(9, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,9};
yystate(8, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(4, Ics, Line, Tlen+1, 17, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $D ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $F, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(8, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,8};
yystate(7, [$T|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, 17, Tlen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $S ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= $U, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(7, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,7};
yystate(6, [$I|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line, Tlen+1, 17, Tlen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $H ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $J, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(6, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,6};
yystate(5, [$S|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, 17, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $R ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $T, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(5, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,5};
yystate(4, [$S|Ics], Line, Tlen, Action, Alen) ->
    yystate(0, Ics, Line, Tlen+1, 17, Tlen);
yystate(4, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $R ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(4, [C|Ics], Line, Tlen, Action, Alen) when C >= $T, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(4, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,4};
yystate(3, [$A|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line, Tlen+1, 17, Tlen);
yystate(3, [C|Ics], Line, Tlen, Action, Alen) when C >= $B, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(3, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,3};
yystate(2, [$R|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line, Tlen+1, 17, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Q ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $S, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(2, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,2};
yystate(1, [$A|Ics], Line, Tlen, Action, Alen) ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(1, [$B|Ics], Line, Tlen, Action, Alen) ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(1, [$C|Ics], Line, Tlen, Action, Alen) ->
    yystate(2, Ics, Line, Tlen+1, 17, Tlen);
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= $D, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 17, Tlen);
yystate(1, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,1};
yystate(0, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(47, Ics, Line, Tlen+1, 9, Tlen);
yystate(0, Ics, Line, Tlen, Action, Alen) ->
    {9,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.


%% yyaction(Action, TokenLength, TokenChars, Line) ->
%%        {token,Token} | {end_token, Token} | skip_token | {error,String}.
%% Generated action function.

yyaction(0, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{integer,YYline,list_to_integer(YYtext)}};
yyaction(1, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    Atom = list_to_atom(YYtext),
    {token,case reserved_word(Atom) of
         true ->
             {Atom,YYline};
         false ->
             {atom,YYline,Atom}
     end};
yyaction(2, YYlen, YYtcs, YYline) ->
    {token,{vsn,YYline}};
yyaction(3, YYlen, YYtcs, YYline) ->
    {token,{typeKwd,YYline}};
yyaction(4, YYlen, YYtcs, YYline) ->
    {token,{state,YYline}};
yyaction(5, YYlen, YYtcs, YYline) ->
    {token,{event,YYline}};
yyaction(6, YYlen, YYtcs, YYline) ->
    {token,{name,YYline}};
yyaction(7, YYlen, YYtcs, YYline) ->
    {token,{info,YYline}};
yyaction(8, YYlen, YYtcs, YYline) ->
    {token,{description,YYline}};
yyaction(9, YYlen, YYtcs, YYline) ->
    {token,{services,YYline}};
yyaction(10, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    S = lists:sublist(YYtext,2,length(YYtext) - 2),
    {token,{string,YYline,S}};
yyaction(11, YYlen, YYtcs, YYline) ->
    {token,{'=>',YYline}};
yyaction(12, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{list_to_atom(YYtext),YYline}};
yyaction(13, YYlen, YYtcs, YYline) ->
    {end_token,{dot,YYline}};
yyaction(14, YYlen, YYtcs, YYline) ->
    {end_token,{dot,YYline}};
yyaction(15, YYlen, YYtcs, YYline) -> skip_token;
yyaction(16, YYlen, YYtcs, YYline) ->
    skip_token;
yyaction(17, YYlen, YYtcs, YYline) ->
    skip_token;
yyaction(_, _, _, _) -> error.
