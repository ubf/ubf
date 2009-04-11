-module(contract_yecc).
-define(THIS_MODULE, contract_yecc).
-export([parse/1, parse_and_scan/1, format_error/1]).

eor(X, nil) -> X;
eor(X, Y) -> {alt, X, Y}.

unwrap({V,_}) -> V;	
unwrap({_,_,V}) -> V.

simplify({Tag,A,nil}) -> A;
simplify(X) -> X.

%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    case catch yeccpars1(Tokens, false, 0, [], []) of
	error ->
	    Errorline =
		if Tokens == [] -> 0; true -> element(2, hd(Tokens)) end,
	    {error,
	     {Errorline, ?THIS_MODULE, "syntax error at or after this line."}};
	Other ->
	    Other
    end.

parse_and_scan({Mod, Fun, Args}) ->
    case apply(Mod, Fun, Args) of
	{eof, _} ->
	    {ok, eof};
	{error, Descriptor, _} ->
	    {error, Descriptor};
	{ok, Tokens, _} ->
	    yeccpars1(Tokens, {Mod, Fun, Args}, 0, [], [])
    end.

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser toplevel.
% Doesn't have to be exported!
return_error(Line, Message) ->
    throw({error, {Line, ?THIS_MODULE, Message}}).


% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {M, F, A}, State, States, Vstack) ->
    case catch apply(M, F, A) of
        {eof, Endline} ->
            {error, {Endline, ?THIS_MODULE, "end_of_file"}};
        {error, Descriptor, Endline} ->
            {error, Descriptor};
        {'EXIT', Reason} ->
            {error, {0, ?THIS_MODULE, Reason}};
        {ok, Tokens, Endline} ->
	    case catch yeccpars1(Tokens, {M, F, A}, State, States, Vstack) of
		error ->
		    Errorline = element(2, hd(Tokens)),
		    {error, {Errorline, ?THIS_MODULE,
			     "syntax error at or after this line."}};
		Other ->
		    Other
	    end
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?THIS_MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({Cat, _, Val}) -> io_lib:format('~w', [Val]);

yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


yeccpars2(0, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, services, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, description, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, info, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, state, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, typeKwd, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, vsn, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(6, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(8, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(9, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(10, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(11, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(12, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(13, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {vsn,unwrap(__4)},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(14, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(15, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(17, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(18, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(19, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  nil,
 yeccpars2(41, __Cat, [19 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(20, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {constant,unwrap(__1)},
 yeccpars2(yeccgoto(primType, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(21, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {integer,unwrap(__1)},
 yeccpars2(yeccgoto(primType, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(annotatedType, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {string,unwrap(__1)},
 yeccpars2(yeccgoto(primType, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(24, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(25, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(26, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(typeSeq, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(27, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(28, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {tuple,__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(primType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(29, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(30, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typeSeq, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(31, __Cat, __Ss,  [__8,__7,__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {type,{unwrap(__3),__7}},
 __Nss = lists:nthtail(7, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(32, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(33, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(34, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {range,unwrap(__1),unwrap(__4)},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(primType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(35, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(36, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(37, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(38, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {constant,unwrap(__1)},
 yeccpars2(yeccgoto(primType, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(39, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __4,
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(annotatedType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(40, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {prim,unwrap(__1)},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(primType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(41, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  eor(__1,__2),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(42, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(43, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  nil,
 yeccpars2(44, __Cat, [43 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(44, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  eor(__2,__3),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(orType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(45, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(46, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {list,__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(primType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(47, event, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(48, '=>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(49, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(transitions, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(50, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(51, '=>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(52, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(53, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {input,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(transition, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(54, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(55, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(outputs, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(56, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(57, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(outputs, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(58, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(59, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {output,__1,unwrap(__3)},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typeAndState, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(60, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {transition,{unwrap(__3),__4}},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(61, event, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(62, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(transitions, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(63, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(64, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {event,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(transition, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(65, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(67, __Cat, [65 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(66, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [unwrap(__1)],
 yeccpars2(yeccgoto(strings, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(67, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(68, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {services,__4},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(70, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(71, __Cat, [70 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(71, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [unwrap(__1)|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(strings, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(72, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(73, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(74, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(75, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {name,unwrap(__4)},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(76, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(77, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(78, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(79, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {info,unwrap(__4)},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(80, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(82, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(83, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {description,unwrap(__4)},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 exit({parser, __Other, missing_state_in_action_table}).

yeccgoto(annotatedType, 17) ->
 19;
yeccgoto(annotatedType, 18) ->
 19;
yeccgoto(annotatedType, 25) ->
 19;
yeccgoto(annotatedType, 29) ->
 19;
yeccgoto(annotatedType, 42) ->
 43;
yeccgoto(annotatedType, 47) ->
 19;
yeccgoto(annotatedType, 52) ->
 19;
yeccgoto(annotatedType, 56) ->
 19;
yeccgoto(annotatedType, 61) ->
 19;
yeccgoto(annotatedType, 63) ->
 19;
yeccgoto(form, 0) ->
 2;
yeccgoto(orType, 19) ->
 41;
yeccgoto(orType, 43) ->
 44;
yeccgoto(outputs, 52) ->
 53;
yeccgoto(outputs, 56) ->
 57;
yeccgoto(primType, 17) ->
 22;
yeccgoto(primType, 18) ->
 22;
yeccgoto(primType, 25) ->
 22;
yeccgoto(primType, 29) ->
 22;
yeccgoto(primType, 37) ->
 39;
yeccgoto(primType, 42) ->
 22;
yeccgoto(primType, 47) ->
 22;
yeccgoto(primType, 52) ->
 22;
yeccgoto(primType, 56) ->
 22;
yeccgoto(primType, 61) ->
 22;
yeccgoto(primType, 63) ->
 22;
yeccgoto(strings, 65) ->
 67;
yeccgoto(strings, 70) ->
 71;
yeccgoto(transition, 47) ->
 49;
yeccgoto(transition, 61) ->
 49;
yeccgoto(transitions, 47) ->
 50;
yeccgoto(transitions, 61) ->
 62;
yeccgoto(type, 17) ->
 24;
yeccgoto(type, 18) ->
 45;
yeccgoto(type, 25) ->
 26;
yeccgoto(type, 29) ->
 26;
yeccgoto(type, 47) ->
 51;
yeccgoto(type, 52) ->
 54;
yeccgoto(type, 56) ->
 54;
yeccgoto(type, 61) ->
 51;
yeccgoto(type, 63) ->
 64;
yeccgoto(typeAndState, 52) ->
 55;
yeccgoto(typeAndState, 56) ->
 55;
yeccgoto(typeSeq, 25) ->
 27;
yeccgoto(typeSeq, 29) ->
 30;
yeccgoto(__Symbol, __State) ->
 exit({__Symbol, __State, missing_in_goto_table}).


