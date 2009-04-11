-module(contracts).

-compile(export_all).
-import(lists, [any/2]).

-export([checkCallback/3, checkIn/3, checkOut/4, isTypeAttr/2, isType/3, getContract/1]).
-include("ubf.hrl").

%% DISABLE -define(enable_fail_debug,true).
-ifdef(enable_fail_debug).
-define(FAIL(X), {fail, X}).
-define(FAILMATCH(), {fail, Result}).
-else.
-define(FAIL(_X), fail).
-define(FAILMATCH(), fail).
-endif. %% -ifndef(enable_fail_debug).

getContract(Mod) ->
    %% io:format("getContract:~p~n",[Mod]),
    File = atom_to_list(Mod) ++ contract_parser:outfileExtension(),
    case file:read_file(File) of
        {ok, Bin} ->
            {ok, ubf:ubf2term(binary_to_list(Bin))};
        E ->
            E
    end.

%%----------------------------------------------------------------------
%% test() ==> test
%% parse({contract, types(), fsm()}) => {ok, internal()} | {error, Why}
%% checkIn(internal(), StateIn, Msg) -> error | {ok, [{S2,M2}]}
%% checkOut(internal(), [{S2,M2}], S2, M2) -> ok | error.


checkIn(Msg, State, Mod) ->
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
        if length(Outs) =:= 0 ->
                Outs1 = [ {InType,OutType,State}
                          || {InType,OutType} <- T1, isType(InType,Msg,Mod) ],
                lists:append(Outs) ++ Outs1;
           true ->
                lists:append(Outs)
        end,
    %% io:format("FSM2=~p~n",[FSM2]),
    FSM2.

checkOut(MsgOut, StateOut, FSM2, Mod) ->
    %% NOTE: ignore input type since tuple size will always be of size
    %% three
    any(fun({_,Type,S2}) when S2 == StateOut ->
                isType(Type,MsgOut,Mod);
           (_) ->
                false
        end, FSM2).

checkCallback(Msg, ThisState, Mod) ->
    T = Mod:contract_state(ThisState),
    Events = [E||{event,E} <- T],
    %% io:format("Events=~p~n",[Events]),
    any(fun(Type) -> isType(Type, Msg, Mod) end, Events).

%%----------------------------------------------------------------------
%% Check type attribute

isTypeAttr(atom,ascii) -> true;
isTypeAttr(atom,asciiprintable) -> true;
isTypeAttr(atom,nonempty) -> true;
isTypeAttr(atom,nonundefined) -> true;
isTypeAttr(binary,ascii) -> true;
isTypeAttr(binary,asciiprintable) -> true;
isTypeAttr(binary,nonempty) -> true;
isTypeAttr(string,ascii) -> true;
isTypeAttr(string,asciiprintable) -> true;
isTypeAttr(string,nonempty) -> true;
isTypeAttr(term,nonempty) -> true;
isTypeAttr(term,nonundefined) -> true;
isTypeAttr(_,_) -> false.

%%----------------------------------------------------------------------
%% Check type

isType(Type, X, Defs) ->
    %% DISABLE io:format("isType(~p,~p,~p)~n",[Type, X, Defs]),
    case (catch check_term(Type, X, 1, Defs)) of
        ?FAILMATCH() ->
            %% DISABLE io:format("***false: ~p~n", [Result]),
            false;
        ok ->
            %% DISABLE io:format("***true~n"),
            true
    end.

check_term(Type, Term, Level, DefTypes) ->
    %% io:format("Check level(~p) ~p isA ~p~n",[Level, Term, Type]),
    check_term1(Type, Term, Level, DefTypes).

check_term1({prim, Type}=Check, X, Level, DefTypes) ->
    %% io:format("looking up def of ~p in ~p~n",[Type, DefTypes]),
    Rhs = locate(Type, DefTypes, X),
    %% io:format("Rhs=~p Level=~p~n",[Rhs,Level]),
    if
        Level < 10010 ->
            check_term(Rhs, X, Level+1, DefTypes);
        true ->
            ?FAIL({Check,X})
    end;
check_term1({prim_optional, Type}=Check, X, Level, DefTypes) ->
    %% io:format("looking up def of ~p in ~p~n",[Type, DefTypes]),
    Rhs = locate(Type, DefTypes, X),
    %% io:format("Rhs=~p Level=~p~n",[Rhs,Level]),
    if
        Level < 10010 ->
            if X =/= undefined ->
                    check_term(Rhs, X, Level+1, DefTypes);
               true ->
                    ok
            end;
        true ->
            ?FAIL({Check,X})
    end;
check_term1({prim_nil, Type}=Check, X, Level, DefTypes) ->
    %% io:format("looking up def of ~p in ~p~n",[Type, DefTypes]),
    _Rhs = locate(Type, DefTypes, X),
    %% io:format("Rhs=~p Level=~p~n",[Rhs,Level]),
    if
        Level < 10010 ->
            if X =:= undefined ->
                    ok;
               true ->
                    ?FAIL({Check,X})
            end;
        true ->
            ?FAIL({Check,X})
    end;
check_term1({prim_required, Type}=Check, X, Level, DefTypes) ->
    %% io:format("looking up def of ~p in ~p~n",[Type, DefTypes]),
    Rhs = locate(Type, DefTypes, X),
    %% io:format("Rhs=~p Level=~p~n",[Rhs,Level]),
    if
        Level < 10010 ->
            if X =/= undefined ->
                    check_term(Rhs, X, Level+1, DefTypes);
               true ->
                    ?FAIL({Check,X})
            end;
        true ->
            ?FAIL({Check,X})
    end;
check_term1({integer,I}, I, _, _) when is_integer(I) ->
    ok;
check_term1({float,F}, F, _, _) when is_float(F) ->
    ok;
check_term1({range,Min,Max}=Check, I, _, _) when is_integer(I) ->
    if I >= Min, I =< Max ->
            ok;
       true ->
            ?FAIL({Check,I})
    end;
check_term1({atom,A}, A, _, _) when is_atom(A) ->
    ok;
check_term1({{atom,Attrs},A}=Check, A, _, _) when is_atom(A) ->
    CheckList = check_term_attrlist(atom,Attrs,A),
    if CheckList =:= [] ->
            ok;
       true ->
            ?FAIL(CheckList)
    end;
check_term1({string,?S(S)}=Check, ?S(S), _, _) when is_list(S) ->
    case is_string(S) of
        true ->
            ok;
        false ->
            ?FAIL({Check,S})
    end;
check_term1({{string,Attrs},?S(S)}=Check, ?S(S), _, _) when is_list(S) ->
    case is_string(S) of
        true ->
            CheckList = check_term_attrlist(string,Attrs,S),
            if CheckList =:= [] ->
                    ok;
               true ->
                    ?FAIL(CheckList)
            end;
        false ->
            ?FAIL({Check,S})
    end;
check_term1({binary,B}, B, _, _) when is_binary(B) ->
    ok;
check_term1({{binary,Attrs},B}=Check, B, _, _) when is_binary(B) ->
    CheckList = check_term_attrlist(binary,Attrs,B),
    if CheckList =:= [] ->
            ok;
       true ->
            ?FAIL(CheckList)
    end;
check_term1({tuple,T}, T, _, _) when is_tuple(T) ->
    ok;
check_term1({tuple,Args}=Check, T, Level, DefTypes) when is_tuple(T) ->
    if
        length(Args) == size(T) ->
            check_term_list(Args, tuple_to_list(T), Level, DefTypes);
        true ->
            ?FAIL({Check})
    end;
check_term1({record,Name,Args}=Check, T, Level, DefTypes) when is_tuple(T) ->
    if
        length(Args)+(1-2) == size(T) ->
            check_term_list([{atom,Name}|tl(tl(Args))], tuple_to_list(T), Level, DefTypes);
        true ->
            ?FAIL({Check})
    end;
check_term1({record_ext,Name,Args}=Check, T, Level, DefTypes) when is_tuple(T) ->
    if
        length(Args)+1 == size(T) ->
            check_term_list([{atom,Name}|Args], tuple_to_list(T), Level, DefTypes);
        true ->
            ?FAIL({Check})
    end;
check_term1({term,T}, T, _, _) ->
    ok;
check_term1({{term,Attrs},T}=Check, T, _, _) ->
    CheckList = check_term_attrlist(term,Attrs,T),
    if CheckList =:= [] ->
            ok;
       true ->
            ?FAIL(CheckList)
    end;
check_term1({void,V}, V, _, _) ->
    ok;
check_term1({list,X}, [H|T], Level, DefTypes) ->
    case check_term(X, H, Level, DefTypes) of
        ok -> check_term({list,X}, T, Level, DefTypes);
        Error -> Error
    end;
check_term1({list,_X}, [], _, _) ->
    ok;
check_term1({alt, A, B}, S, Level, DefTypes) ->
    case check_term(A, S, Level, DefTypes) of
        ok  -> ok;
        _   -> check_term(B, S, Level, DefTypes)
    end;
check_term1({concat, A, B}=Check, L, Level, DefTypes) when is_list(L) ->
    case check_term_concatlist(A, 0, B, L, Level, DefTypes) of
        {ok,[]} -> ok;
        {ok,_} -> ?FAIL({Check,L});
        {Error,_} -> Error
    end;
check_term1({list_optional,X}=Check, L, Level, DefTypes) when is_list(L) ->
    if
        length(L) =:= 0 orelse length(L) =:= 1 ->
            check_term({list,X}, L, Level, DefTypes);
        true ->
            ?FAIL({Check,L})
    end;
check_term1({list_nil,X}=Check, L, Level, DefTypes) when is_list(L) ->
    if
        length(L) =:= 0 ->
            check_term({list,X}, L, Level, DefTypes);
        true ->
            ?FAIL({Check,L})
    end;
check_term1({list_required,X}=Check, L, Level, DefTypes) when is_list(L) ->
    if
        length(L) =:= 1 ->
            check_term({list,X}, L, Level, DefTypes);
        true ->
            ?FAIL({Check,L})
    end;
check_term1({list_required_and_repeatable,X}=Check, L, Level, DefTypes) when is_list(L) ->
    if
        length(L) =/= 0 ->
            check_term({list,X}, L, Level, DefTypes);
        true ->
            ?FAIL({Check,L})
    end;
check_term1(Check, X, _Level, _DefTypes) ->
    %% io:format("~p isnot ~p~n", [Check, X]),
    %% exit({Y,isNotA, X}).
    ?FAIL({last,Check,X}).

check_term_list([H|T], [H1|T1], Level, DefTypes) ->
    case check_term(H, H1, Level, DefTypes) of
        ok ->
            check_term_list(T, T1, Level, DefTypes);
        Error ->
            ?FAIL([Error,{H,H1}])
    end;
check_term_list([], [], _, _) ->
    ok.

check_term_concatlist({concat,A1,A2}, 0, B, L, Level, DefTypes) ->
    case check_term_concatlist(A1, 0, A2, L, Level, DefTypes) of
        {ok,Rest} ->
            check_term_concatlist(B, 0, Rest, Level, DefTypes);
        {Error,_} ->
            {Error,L}
    end;
check_term_concatlist({AType,_} = A, ACnt, B, [H|T] = L, Level, DefTypes)
  when AType =:= list orelse AType =:= list_optional ->
    case check_term(A, [H], Level, DefTypes) of
        ok ->
            check_term_concatlist(A, ACnt+1, B, T, Level, DefTypes);
        _ ->
            check_term_concatlist(B, 0, L, Level, DefTypes)
    end;
check_term_concatlist({list_nil,_} = _A, 0, B, [_H|_T] = L, Level, DefTypes) ->
    check_term_concatlist(B, 0, L, Level, DefTypes);
check_term_concatlist({list_required,_} = A, 0, B, [H|T] = L, Level, DefTypes) ->
    case check_term(A, [H], Level, DefTypes) of
        ok ->
            check_term_concatlist(B, 0, T, Level, DefTypes);
        Error ->
            {Error,L}
    end;
check_term_concatlist({list_required_and_repeatable,_} = A, ACnt, B, [H|T] = L, Level, DefTypes) ->
    case check_term(A, [H], Level, DefTypes) of
        ok ->
            check_term_concatlist(A, ACnt+1, B, T, Level, DefTypes);
        Error ->
            if ACnt =/= 0 ->
                    check_term_concatlist(B, 0, L, Level, DefTypes);
               true ->
                    {Error, L}
            end
    end;
check_term_concatlist({AType,_}, _, _, [], _, _)
  when AType =:= list orelse AType =:= list_optional ->
    {ok,[]};
check_term_concatlist({list_nil,_}, 0, _, [], _, _) ->
    {ok,[]};
check_term_concatlist({list_required,_}=Check, 0, _, [], _, _) ->
    {?FAIL(Check),[]};
check_term_concatlist({list_required_and_repeatable,_}=Check, ACnt, _, [], _, _) ->
    if ACnt =/= 0 ->
            {ok,[]};
       true ->
            {?FAIL(Check),[]}
    end.

check_term_concatlist({concat,B1,B2}, 0, L, Level, DefTypes) ->
    check_term_concatlist(B1, 0, B2, L, Level, DefTypes);
check_term_concatlist({BType,_} = B, BCnt, [H|T] = L, Level, DefTypes)
  when BType =:= list orelse BType =:= list_optional ->
    case check_term(B, [H], Level, DefTypes) of
        ok ->
            check_term_concatlist(B, BCnt+1, T, Level, DefTypes);
        _ ->
            {ok,L}
    end;
check_term_concatlist({list_nil,_} = _B, 0, L, _Level, _DefTypes) ->
    {ok,L};
check_term_concatlist({list_required,_} = B, 0, [H|T] = L, Level, DefTypes) ->
    case check_term(B, [H], Level, DefTypes) of
        ok ->
            {ok,T};
        Error ->
            {Error,L}
    end;
check_term_concatlist({list_required_and_repeatable,_} = B, BCnt, [H|T] = L, Level, DefTypes) ->
    case check_term(B, [H], Level, DefTypes) of
        ok ->
            check_term_concatlist(B, BCnt+1, T, Level, DefTypes);
        Error ->
            if BCnt =/= 0 ->
                    {ok,L};
               true ->
                    {Error, L}
            end
    end;
check_term_concatlist({BType,_}, _, [], _, _)
  when BType =:= list orelse BType =:= list_optional ->
    {ok,[]};
check_term_concatlist({list_nil,_}, 0, [], _, _) ->
    {ok,[]};
check_term_concatlist({list_required,_}=Check, 0, [], _, _) ->
    {?FAIL(Check),[]};
check_term_concatlist({list_required_and_repeatable,_}=Check, BCnt, [], _, _) ->
    if BCnt =/= 0 ->
            {ok,[]};
       true ->
            {?FAIL(Check),[]}
    end.

check_term_attrlist(Type, Attrs, Val) ->
    [ {Type,Attr,Val} || Attr <- Attrs, not check_term_attr(Type,Attr,Val) ].

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

locate(Type, Mod, X) ->
    case lists:member(Type, contract_parser:preDefinedTypes()) of
        true ->
            {Type, X};
        false ->
            element(1, Mod:contract_type(Type))
    end.

is_string(A) when is_atom(A) ->
    is_string(atom_to_list(A));
is_string([H|T]) when is_integer(H), H < 256, H > -1 ->
    is_string(T);
is_string(<<H:8,T>>) when is_integer(H), H < 256, H > -1 ->
    is_string(T);
is_string([]) -> true;
is_string(<<>>) -> true;
is_string(_)  -> false.

is_ascii(A) when is_atom(A) ->
    is_ascii(atom_to_list(A));
is_ascii([H|T]) when is_integer(H), H < 128, H > -1 ->
    is_ascii(T);
is_ascii(<<H:8,T>>) when is_integer(H), H < 128, H > -1 ->
    is_ascii(T);
is_ascii([]) -> true;
is_ascii(<<>>) -> true;
is_ascii(_)  -> false.

is_asciiprintable(A) when is_atom(A) ->
    is_asciiprintable(atom_to_list(A));
is_asciiprintable([H|T]) when is_integer(H), H < 127, H > 31 ->
    is_asciiprintable(T);
is_asciiprintable(<<H:8,T>>) when is_integer(H), H < 127, H > 31 ->
    is_asciiprintable(T);
is_asciiprintable([]) -> true;
is_asciiprintable(<<>>) -> true;
is_asciiprintable(_)  -> false.

is_nonempty('') -> false;
is_nonempty([]) -> false;
is_nonempty(<<>>) -> false;
is_nonempty({}) -> false;
is_nonempty(_) -> true.

is_nonundefined(undefined) -> false;
is_nonundefined(_) -> true.
