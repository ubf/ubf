%%% The MIT License
%%%
%%% Copyright (C) 2011 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Contract implementation: compare a term against a contract.
%%%
%%% See the function checkType/3 for assistance on checking if a term
%%% does/does not break a contract.

-module(contracts).

%%-compile(export_all).
-export([checkEventOut/3, checkEventIn/3, checkRPCIn/3, checkRPCOut/4]).
-export([isTypeAttr/2, isType/3, checkType/3]).
-include("ubf.hrl").


-define(FAIL(_X), false).


%%----------------------------------------------------------------------
%% test() ==> test
%% parse({contract, types(), fsm()}) => {ok, internal()} | {error, Reason}
%% checkRPCIn(internal(), StateIn, Msg) -> error | {ok, [{S2,M2}]}
%% checkRPCOut(internal(), [{S2,M2}], S2, M2) -> ok | error.

checkRPCIn(Msg, State, Mod) ->
    %% Check that the Msg is in the set of
    %% incoming states
    %% io:format("check: Msg=~p, State=~p, plugin=~p~n",[Msg,State,Mod]),
    T = Mod:contract_state(State),
    %% io:format("T=~p~n",[T]),
    T1 = Mod:contract_anystate(),
    %% io:format("T1=~p~n",[T1]),
    %% NOTE: replace "output" with input type since tuple size will
    %% always be of size three
    Outs = [ [ erlang:setelement(1,O,Type) || O <- Out ]
             || {input,Type,Out} <- T, isType(Type,Msg,Mod) ],
    FSM2 =
        case Outs of
            [] ->
                [ {InType,OutType,State}
                  || {InType,OutType} <- T1, isType(InType,Msg,Mod) ];
           _ ->
                lists:append(Outs)
        end,
    %% io:format("FSM2=~p~n",[FSM2]),
    FSM2.

checkRPCOut(MsgOut, StateOut, FSM2, Mod) ->
    %% NOTE: ignore input type since tuple size will always be of size
    %% three
    lists:any(fun({_,Type,S2}) when S2 == StateOut ->
                isType(Type,MsgOut,Mod);
           (_) ->
                false
        end, FSM2).

checkEventOut(Msg, ThisState, Mod) ->
    T = Mod:contract_state(ThisState),
    T1 = Mod:contract_anystate(),
    Events = [ E || {event_out,E} <- T ] ++ [ E || {event_out,E} <- T1 ],
    %% io:format("Events=~p~n",[Events]),
    lists:any(fun(Type) -> isType(Type, Msg, Mod) end, Events).

checkEventIn(Msg, ThisState, Mod) ->
    T = Mod:contract_state(ThisState),
    T1 = Mod:contract_anystate(),
    Events = [ E || {event_out,E} <- T ] ++ [ E || {event_out,E} <- T1 ],
    %% io:format("Events=~p~n",[Events]),
    lists:any(fun(Type) -> isType(Type, Msg, Mod) end, Events).

%%----------------------------------------------------------------------
%% Check type attribute

isTypeAttr(atom,ascii) -> true;
isTypeAttr(atom,asciiprintable) -> true;
isTypeAttr(atom,nonempty) -> true;
isTypeAttr(atom,nonundefined) -> true;
isTypeAttr(binary,ascii) -> true;
isTypeAttr(binary,asciiprintable) -> true;
isTypeAttr(binary,nonempty) -> true;
isTypeAttr(list,nonempty) -> true;
isTypeAttr(proplist,nonempty) -> true;
isTypeAttr(string,ascii) -> true;
isTypeAttr(string,asciiprintable) -> true;
isTypeAttr(string,nonempty) -> true;
isTypeAttr(term,nonempty) -> true;
isTypeAttr(term,nonundefined) -> true;
isTypeAttr(tuple,nonempty) -> true;
isTypeAttr(_,_) -> false.

%%----------------------------------------------------------------------
%% Check type

isType(Type, X, Mod) ->
    %% DISABLE io:format("isType(~p,~p,~p)~n",[Type, X, Mod]),
    case check_term(Type, X, 1, Mod) of
        true ->
            %% DISABLE io:format("***true~n"),
            true;
        false ->
            %% DISABLE io:format("***false: ~p~n", [Result]),
            false
    end.

%% alt
check_term({alt, A, B}, X, Level, Mod) ->
    check_term(A, X, Level, Mod) orelse check_term(B, X, Level, Mod);
%% prim
check_term({prim, Min, Max, Type}=_Check, X, Level, Mod) ->
    %% NOTE: hard-coded max level of 10010
    if Level < 10010 ->
            TypeDef =
                case Type of
                    {predef,_} ->
                        Type;
                    _ ->
                        element(1, Mod:contract_type(Type))
                end,
            case check_term_prim(Min, Max, TypeDef, X, Level+1, Mod) of
                true ->
                    true;
                false ->
                    ?FAIL({Check,X})
            end;
       true ->
            ?FAIL({maxlevel,X})
    end;
%% tuple
check_term({tuple,Args}=_Check, X, Level, Mod) ->
    if length(Args) =:= tuple_size(X) ->
            case check_term_seq(Args, tuple_to_list(X), Level, Mod) of
                true ->
                    true;
                false ->
                    ?FAIL({Check,X})
            end;
       true ->
            ?FAIL({Check,X})
    end;
%% record
check_term({record,Name,Args}=_Check, X, Level, Mod) ->
    if length(Args)+(1-2) =:= tuple_size(X) ->
            case check_term_seq([{atom,Name}|tl(tl(Args))], tuple_to_list(X), Level, Mod) of
                true ->
                    true;
                false ->
                    ?FAIL({Check,X})
            end;
       true ->
            ?FAIL({Check,X})
    end;
check_term({record_ext,Name,Args}=_Check, X, Level, Mod) ->
    if length(Args)+1 =:= tuple_size(X) ->
            case check_term_seq([{atom,Name}|Args], tuple_to_list(X), Level, Mod) of
                true ->
                    true;
                false ->
                    ?FAIL({Check,X})
            end;
       true ->
            ?FAIL({Check,X})
    end;
%% list
check_term({list,Min,Max,Args}=_Check, X, Level, Mod) ->
    if is_list(X) ->
            Len = length(X),
            if Len < Min orelse (Max =/= infinity andalso Len > Max) ->
                    ?FAIL({Check,X});
               true ->
                    case check_term_list(Args, X, Level, Mod) of
                        true ->
                            true;
                        false ->
                            ?FAIL({Check,X})
                    end
            end;
       true ->
            ?FAIL({Check,X})
    end;
%% range
check_term({range, Min, Max}=_Check, X, _Level, _Mod) ->
    if is_integer(X) ->
            case check_term_range(Min, Max, X) of
                true ->
                    true;
                false ->
                    ?FAIL({Check,X})
            end;
       true ->
            ?FAIL({Check,X})
    end;
%% atom
check_term({atom, Y}=_Check, X, _Level, _Mod) ->
    if Y =:= X andalso is_atom(Y) ->
            true;
       true ->
            ?FAIL({Check,X})
    end;
%% binary
check_term({binary, Y}=_Check, X, _Level, _Mod) ->
    if Y =:= X andalso is_binary(Y) ->
            true;
       true ->
            ?FAIL({Check,X})
    end;
%% float
check_term({float, Y}=_Check, X, _Level, _Mod) ->
    if Y =:= X andalso is_float(Y) ->
            true;
       true ->
            ?FAIL({Check,X})
    end;
%% integer
check_term({integer, Y}=_Check, X, _Level, _Mod) ->
    if Y =:= X andalso is_integer(Y) ->
            true;
       true ->
            ?FAIL({Check,X})
    end;
%% string
check_term({string, {'#S', Y0}=Y}=_Check, X, _Level, _Mod) ->
    if Y =:= X andalso is_list(Y0) ->
            true;
       true ->
            ?FAIL({Check,X})
    end;
%% predef
check_term({predef, Args}=_Check, X, _Level, _Mod) ->
    case check_term_predef(Args, X) of
        true ->
            true;
        false ->
            ?FAIL({Check,X})
    end;
%% abnf
check_term(Check, X, Level, Mod) when is_binary(X) ->
    case contracts_abnf:check_binary(Check, X, Level, Mod) of
        true ->
            true;
        false ->
            ?FAIL({Check,X})
    end;
%% otherwise, fail
check_term(_Check, _X, _Level, _Mod) ->
    %% io:format("~p isnot ~p~n", [Check, X]),
    %% exit({Y,isNotA, X}).
    ?FAIL({last,_Check,_X}).


%% check_term_prim
check_term_prim(1, 1, TypeDef, X, Level, Mod) ->
    check_term(TypeDef, X, Level, Mod);
check_term_prim(0, 1, TypeDef, X, Level, Mod) ->
    if X =/= undefined ->
            check_term(TypeDef, X, Level, Mod);
       true ->
            true
    end;
check_term_prim(0, 0, _TypeDef, X, _Level, _Mod) ->
    X =:= undefined.


%% check_term_seq
check_term_seq([], [], _Level, _Mod) ->
    true;
check_term_seq(_Args, [], _Level, _Mod) ->
    false;
check_term_seq([], _L, _Level, _Mod) ->
    false;
check_term_seq([H1|T1], [H2|T2], Level, Mod) ->
    check_term(H1, H2, Level, Mod) andalso check_term_seq(T1, T2, Level, Mod).


%% check_term_list
check_term_list(Args, List, Level, Mod) ->
    lists:all(fun (X) -> check_term(Args, X, Level, Mod) end, List).


%% check_term_range
check_term_range(infinity, Max, X) ->
    X =< Max;
check_term_range(Min, infinity, X) ->
    Min =< X;
check_term_range(Min, Max, X) ->
    Min =< X andalso X =< Max.


%% check_term_predef
check_term_predef(atom, X) ->
    is_atom(X);
check_term_predef(binary, X) ->
    is_binary(X);
check_term_predef(float, X) ->
    is_float(X);
check_term_predef(integer, X) ->
    is_integer(X);
check_term_predef(list, X) ->
    is_list(X);
check_term_predef(proplist, X) ->
    case X of
        {'#P', Y} when is_list(Y) ->
            is_proplist(Y);
        _ ->
            false
    end;
check_term_predef(string, X) ->
    case X of
        {'#S', Y} when is_list(Y) ->
            is_string(Y);
        _ ->
            false
    end;
check_term_predef(term, _X) ->
    true;
check_term_predef(tuple, X) ->
    is_tuple(X);
check_term_predef(void, _X) ->
    true;
check_term_predef({atom,Attrs}, X) ->
    is_atom(X) andalso check_term_attrlist(atom,Attrs,X);
check_term_predef({binary,Attrs}, X) ->
    is_binary(X) andalso check_term_attrlist(binary,Attrs,X);
check_term_predef({list,Attrs}, X) ->
    is_list(X) andalso check_term_attrlist(list,Attrs,X);
check_term_predef({proplist,Attrs}, X) ->
    case X of
        {'#P', Y} when is_list(Y) ->
            is_proplist(Y) andalso check_term_attrlist(proplist,Attrs,X);
        _ ->
            false
    end;
check_term_predef({string,Attrs}, X) ->
    case X of
        {'#S', Y} when is_list(Y) ->
            is_string(Y) andalso check_term_attrlist(string,Attrs,X);
        _ ->
            false
    end;
check_term_predef({term,Attrs}, X) ->
    check_term_attrlist(term,Attrs,X);
check_term_predef({tuple,Attrs}, X) ->
    is_tuple(X) andalso check_term_attrlist(tuple,Attrs,X).


%% check_term_attrlist
check_term_attrlist(Type, Attrs, Val) ->
    [] == [ {Type,Attr,Val} || Attr <- Attrs, not check_term_attr(Type,Attr,Val) ].


%% check_term_attr
check_term_attr(Type,ascii,Val) ->
    isTypeAttr(Type,ascii) andalso is_ascii(Val);
check_term_attr(Type,asciiprintable,Val) ->
    isTypeAttr(Type,asciiprintable) andalso is_asciiprintable(Val);
check_term_attr(Type,nonempty,Val) ->
    isTypeAttr(Type,nonempty) andalso is_nonempty(Val);
check_term_attr(Type,nonundefined,Val) ->
    isTypeAttr(Type,nonundefined) andalso is_nonundefined(Val);
check_term_attr(_,_,_) ->
    false.


%% is_string
is_string([H|T]) when is_integer(H), H < 256, H > -1 ->
    is_string(T);
is_string([]) -> true;
is_string(_)  -> false.


%% is_proplist
is_proplist([P|T]) when tuple_size(P) =:= 2 ->
    is_proplist(T);
is_proplist([]) -> true;
is_proplist(_)  -> false.


%% is_ascii
is_ascii(A) when is_atom(A) ->
    is_ascii(atom_to_list(A));
is_ascii([H|T]) when is_integer(H), H < 128, H > -1 ->
    is_ascii(T);
is_ascii(<<H:8,T/binary>>) when is_integer(H), H < 128, H > -1 ->
    is_ascii(T);
is_ascii([]) -> true;
is_ascii(<<>>) -> true;
is_ascii(_)  -> false.


%% is_asciiprintable
is_asciiprintable(A) when is_atom(A) ->
    is_asciiprintable(atom_to_list(A));
is_asciiprintable([H|T]) when is_integer(H), H < 127, H > 31 ->
    is_asciiprintable(T);
is_asciiprintable(<<H:8,T/binary>>) when is_integer(H), H < 127, H > 31 ->
    is_asciiprintable(T);
is_asciiprintable([]) -> true;
is_asciiprintable(<<>>) -> true;
is_asciiprintable(_)  -> false.


%% is_nonempty
is_nonempty('') -> false;
is_nonempty([]) -> false;
is_nonempty(<<>>) -> false;
is_nonempty({}) -> false;
is_nonempty(_) -> true.


%% is_nonundefined
is_nonundefined(undefined) -> false;
is_nonundefined(_) -> true.


%% @doc Given a contract type name, a term to check against that
%% contract type, and a contract module name, verify the term against
%% that contract\'s type.
%%
%% Example usage from the irc_plugin.con contract:
%%
%% ------
%% 1> contracts:checkType(ok, ok, irc_plugin).
%% 2> contracts:checkType(bool, true, irc_plugin).
%% 3> contracts:checkType(nick, {'#S', "foo"}, irc_plugin).
%% 4> contracts:checkType(joinEvent, {joins, {'#S', "nck"}, {'#S', "grp"}}, irc_plugin).
%% 5> contracts:checkType(joinEvent, {joins, {'#S', "nck"}, {'#S', bad_atom}}, irc_plugin).
%% ------
%%
%% NOTE: This is a brute-force function, but it works, mostly.  Don\'t
%% try to have a computer parse the output in error cases: the failure
%% output is meant only for human eyes.

%% @TODO implementation needs updating for new primitives?

-spec checkType(atom(), term(), module()) -> yup | term().
checkType(HumanType, Term, Mod) ->
    case (catch Mod:contract_type(HumanType)) of
        {'EXIT', {function_clause, _}} ->
            type_not_in_contract;
        {{record, _, _}, []} ->
            checkType2({prim, 1, 1, HumanType}, Term, Mod);
        {{tuple, _}, []} ->
            checkType2({prim, 1, 1, HumanType}, Term, Mod);
        {{alt, TypeA, TypeB}, []} ->
            ResA = checkType2(TypeA, Term, Mod),
            ResB = checkType2(TypeB, Term, Mod),
            if ResA == yup; ResB == yup ->
                    yup;
               true ->
                    {bad_alternative, HumanType, Term}
            end;
        {ContractTypeMaybe, []} ->
            checkType2(ContractTypeMaybe, Term, Mod);
        _ ->
            case checkType2(HumanType, Term, Mod) of
                yup ->
                    yup;
                Res ->
                    {badType, bug_or_bad_input, Res}
            end
    end.

checkType2({prim, _, _, HumanType} = Type, Term, Mod) ->
    case (catch Mod:contract_type(HumanType)) of
        {{record, HumanType, Elements}, []} ->
            case isType(Type, Term, Mod) of
                true ->
                    yup;
                false ->
                    RecTypes = [{atom, HumanType}|tl(tl(Elements))],
                    bad_zip(RecTypes, tuple_to_list(Term), Mod)
            end;
        {{alt, TypeA, TypeB}, []} ->
            ResA = checkType2(TypeA, Term, Mod),
            ResB = checkType2(TypeB, Term, Mod),
            if ResA == yup; ResB == yup ->
                    yup;
               true ->
                    {badType, HumanType, Term}
            end;
        {Something, []} ->
            checkType2(Something, Term, Mod);
        {'EXIT', {function_clause, _}} ->
            case isType(Type, Term, Mod) of
                true ->
                    yup;
                false ->
                    {badType, {type_wanted, Type, Term}}
            end
    end;
checkType2({tuple, TupleTypes} = _Type, Term, _Mod)
  when length(TupleTypes) =/= size(Term); not is_tuple(Term) ->
    {badTupleSize, Term, expected, length(TupleTypes)};
checkType2({tuple, TupleTypes} = Type, Term, Mod) ->
    case isType(Type, Term, Mod) of
        true ->
            yup;
        false ->
            bad_zip(TupleTypes, tuple_to_list(Term), Mod)
    end;
checkType2(Type, Term, Mod) ->
    case isType(Type, Term, Mod) of
        true ->
            yup;
        false ->
            checkType_investigate_deeper(Type, Term, Mod)
    end.

bad_zip(TypesList, TermList, Mod) ->
    TpsTrm = lists:zip3(TypesList, TermList, lists:seq(1, length(TermList))),
    Items = [{isType(Type, Part, Mod), Type, _Pos} ||
                {Type, Part, _Pos} <- TpsTrm],
    [{badType, WantedType,
      checkType2(WantedType, lists:nth(Pos, TermList), Mod)} ||
        {false, WantedType, Pos} <- Items].

%% checkType_investigate_deeper({prim, _} = Type, Term, Mod) ->
%%     INFINITE LOOP, don\'t do this....
%%     checkType2(Type, Term, Mod);
checkType_investigate_deeper({list, _Min, _Max, Type}, TermL, Mod) ->
    if is_list(TermL) ->
            bad_zip([Type || _ <- TermL], TermL, Mod);
       true ->
            {expecting_list_but_got, TermL}
    end;
checkType_investigate_deeper(Type, Term, _Mod) ->
    {badType, Type, Term}.
