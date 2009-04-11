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
yeccpars2(1, description, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, info, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, state, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, typeKwd, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, vsn, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(6, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(8, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(9, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(10, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(11, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(12, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {vsn,unwrap(__4)},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(13, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(14, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(15, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(17, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(18, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  nil,
 yeccpars2(40, __Cat, [18 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(19, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {atom,unwrap(__1)},
 yeccpars2(yeccgoto(primType, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(20, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {integer,unwrap(__1)},
 yeccpars2(yeccgoto(primType, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(21, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(annotatedType, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {string,unwrap(__1)},
 yeccpars2(yeccgoto(primType, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(23, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(24, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(25, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(typeSeq, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(26, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(27, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {tuple,__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(primType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(28, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(29, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typeSeq, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(30, __Cat, __Ss,  [__8,__7,__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {type,{unwrap(__3),__7}},
 __Nss = lists:nthtail(7, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(31, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(32, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(33, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {range,unwrap(__1),unwrap(__4)},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(primType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(34, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(35, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(36, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(37, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {atom,unwrap(__1)},
 yeccpars2(yeccgoto(primType, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(38, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __4,
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(annotatedType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(39, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {prim,unwrap(__1)},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(primType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(40, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  eor(__1,__2),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(41, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(42, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  nil,
 yeccpars2(43, __Cat, [42 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(43, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  eor(__2,__3),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(orType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(44, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(45, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {list,__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(primType, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(46, event, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(47, '=>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(48, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(transitions, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(49, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(50, '=>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(51, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(52, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {input,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(transition, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(53, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(54, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(outputs, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(55, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(56, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(outputs, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(57, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(58, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {output,__1,unwrap(__3)},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typeAndState, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(59, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {transition,{unwrap(__3),__4}},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(60, event, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(61, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(transitions, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(62, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(63, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {event,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(transition, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(64, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(65, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(66, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(67, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {name,unwrap(__4)},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(68, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [69 | __Ss], [__T | __Stack]);
yeccpars2(69, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(70, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(71, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {info,unwrap(__4)},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
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
 __Val =  {description,unwrap(__4)},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 exit({parser, __Other, missing_state_in_action_table}).

yeccgoto(annotatedType, 16) ->
 18;
yeccgoto(annotatedType, 17) ->
 18;
yeccgoto(annotatedType, 24) ->
 18;
yeccgoto(annotatedType, 28) ->
 18;
yeccgoto(annotatedType, 41) ->
 42;
yeccgoto(annotatedType, 46) ->
 18;
yeccgoto(annotatedType, 51) ->
 18;
yeccgoto(annotatedType, 55) ->
 18;
yeccgoto(annotatedType, 60) ->
 18;
yeccgoto(annotatedType, 62) ->
 18;
yeccgoto(form, 0) ->
 2;
yeccgoto(orType, 18) ->
 40;
yeccgoto(orType, 42) ->
 43;
yeccgoto(outputs, 51) ->
 52;
yeccgoto(outputs, 55) ->
 56;
yeccgoto(primType, 16) ->
 21;
yeccgoto(primType, 17) ->
 21;
yeccgoto(primType, 24) ->
 21;
yeccgoto(primType, 28) ->
 21;
yeccgoto(primType, 36) ->
 38;
yeccgoto(primType, 41) ->
 21;
yeccgoto(primType, 46) ->
 21;
yeccgoto(primType, 51) ->
 21;
yeccgoto(primType, 55) ->
 21;
yeccgoto(primType, 60) ->
 21;
yeccgoto(primType, 62) ->
 21;
yeccgoto(transition, 46) ->
 48;
yeccgoto(transition, 60) ->
 48;
yeccgoto(transitions, 46) ->
 49;
yeccgoto(transitions, 60) ->
 61;
yeccgoto(type, 16) ->
 23;
yeccgoto(type, 17) ->
 44;
yeccgoto(type, 24) ->
 25;
yeccgoto(type, 28) ->
 25;
yeccgoto(type, 46) ->
 50;
yeccgoto(type, 51) ->
 53;
yeccgoto(type, 55) ->
 53;
yeccgoto(type, 60) ->
 50;
yeccgoto(type, 62) ->
 63;
yeccgoto(typeAndState, 51) ->
 54;
yeccgoto(typeAndState, 55) ->
 54;
yeccgoto(typeSeq, 24) ->
 26;
yeccgoto(typeSeq, 28) ->
 29;
yeccgoto(__Symbol, __State) ->
 exit({__Symbol, __State, missing_in_goto_table}).


