-module(contracts).

-compile(export_all).
-import(lists, [any/2, filter/2, foldl/3, member/2, map/2]).

-export([checkCallback/3, checkIn/3, checkOut/4, isType/3, getContract/1]).
-include("contract.hrl").

getContract(Mod) ->
    io:format("getContract:~p~n",[Mod]),
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
    Outs = [Out||{input, Type, Out} <- T,
		 isType(Type, Msg, Mod)],
    lists:append(Outs).

checkOut(MsgOut, StateOut, FSM2, Mod) ->
    any(fun({output,Type,S2}) when S2 == StateOut -> 
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
%% Check type

isType(Type, X, Defs) ->
    %% io:format("isType(~p,~p,~p)~n",[Type, X, Defs]),
    case (catch check_term(Type, X, 1, Defs)) of
	{'EXIT', Why} ->
	    %% io:format("***false~n"),
	    false;
	_ ->
	    %% io:format("***true~n"),
	    true
    end.

check_term(Type, Term, Level, DefTypes) ->
    %% io:format("Check level(~p) ~p isA ~p~n",[Level, Term, Type]),
    check_term1(Type, Term, Level, DefTypes).

check_term1({integer,I}, I, _, _) -> ok;
check_term1({atom,A}, A, _, _) -> ok;
check_term1({string,S}, S, _, _) -> ok;
%% check_term1({list, X}, {'#L', Y}, Level, DefTypes) ->
%%    check_term({list,X}, Y, Level, DefTypes);
check_term1({list,X}, [H|T], Level, DefTypes) ->
    case check_term(X, H, Level, DefTypes) of
	ok -> check_term({list, X}, T, Level, DefTypes);
	Error -> Error
    end;
check_term1({list,X}, [], _, _) ->
    ok;
check_term1({alt, A, B}, S, Level, DefTypes) ->
    case check_term(A, S, Level, DefTypes) of
	ok  -> ok;
	_   -> check_term(B, S, Level, DefTypes)
    end;
check_term1({tuple, Args}, L, Level, DefTypes) ->
    if
	length(Args) == size(L) ->
	    check_term_list(Args, tuple_to_list(L), Level, DefTypes);
	true ->
	    exit(tupleSize)
    end;
check_term1({prim,int}, I, _, _) when integer(I) ->
    ok;
check_term1({range,Min,Max}, I, _, _) when integer(I),
					   I >= Min, I =< Max ->
    ok;
check_term1({prim,int}, I, _, _) ->
    exit({I,isNot,integer});
check_term1({prim, binary},I, _, _) when binary(I) ->
    ok;
check_term1(P={prim,string}, {'#S',S}, Level, DefTypes) ->
    case is_string(S) of
	true -> ok;
	false ->
	    exit({S, isNota, string})
    end;
check_term1({prim, void}, _, _, _) ->
    ok;
check_term1({prim, term}, _, _, _) ->
    ok;
check_term1({prim, Type}, X, Level, DefTypes) ->
    %% io:format("looking up def of ~p in ~p~n",[Type, DefTypes]),
    Rhs = locate(Type, DefTypes),
    %% io:format("Rhs=~p Level=~p~n",[Rhs,Level]),
    if
	Level < 10010 ->
	    check_term(Rhs, X, Level+1, DefTypes);
	true ->
	    false
    end;
check_term1(X, Y, Level, DefTypes) ->
    %% io:format("~p isnot ~p~n", [Y, X]),
    exit({Y,isNotA, X}).

check_term_list([H|T], [H1|T1], Level, DefTypes) ->
    case check_term(H, H1, Level, DefTypes) of
	ok ->
	    check_term_list(T, T1, Level, DefTypes);
	Error ->
	    Error
    end;
check_term_list([], [], _, _) ->
    ok.

locate(Type, Mod) ->
    Mod:contract_type(Type).

is_string([H|T]) when integer(H), H < 256, H > -1 -> 
    is_string(T);
is_string([]) -> true;
is_string(_)  -> false.

    




