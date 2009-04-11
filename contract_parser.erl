-module(contract_parser).

%% parse contract language
%% Copyright 2002 Joe Armstrong (joe@sics.se) 
%% Documentation http:://www.sics.se/~joe/ubf.html

-export([parse_transform/2,
	 test/0, make/0, make_lex/0, make_yecc/0, outfileExtension/0, 
	 file/1, batch/1]).

-import(lists, [filter/2, foreach/2, map/2, member/2, foldl/3]).
-include("contract.hrl").

parse_transform(In, Opts) ->
    %% io:format("In:~p~n",[In]),
    Out = case [X||{attribute,_,add_contract,X} <- In] of
	      [File] ->
		  case file1(File ++ ".con") of
		      {ok, Contract} ->
			  io:format("Contact added:~n"),
			  {Export, Fns} = make_code(Contract),
			  In1 = merge_in_code(In, Export, Fns),
			  %% foreach(fun(I) -> 
			  %% io:format("~s~n",[erl_pp:form(I)]) end, In1),
			  In1;
		      {error, Why} ->
			  io:format("Error in contract:~p~n", [Why]),
			  exit(error)
		  end;
	      [] ->
		  In
	  end,
    Out.

test() ->
     file("pics_plugin.con").

make_code(C) ->
    F1 = {function,0,contract_name,0,
	  [{clause,0,[],[],[{string,0,C#contract.name}]}]},
    F2 = {function,0,contract_info,0,
	  [{clause,0,[],[],[{string,0,C#contract.info}]}]},
    F3 = {function,0,contract_description,0,
	  [{clause,0,[],[],[{string,0,C#contract.description}]}]},
    Tmp1 = map(fun({Type,Val}) ->
		    {clause,1,[{atom,0,Type}],[],[erl_parse:abstract(Val)]}
	    end,  C#contract.types),
    F4 = {function,0,contract_type,1, Tmp1},
    Tmp2 = map(fun({State,Val}) ->
		       {clause,1,[{atom,0,State}],[],[erl_parse:abstract(Val)]}
	       end,  C#contract.transitions),
    F5 = {function,0,contract_state,1, Tmp2},
    TypeNames = map(fun({Type,_}) -> Type end, C#contract.types),
    F6 = {function,0,contract_types,0,
	  [{clause,0,[],[],[erl_parse:abstract(TypeNames, 0)]}]},
    StateNames = map(fun({State,_}) -> State end, C#contract.transitions),
    F7 = {function,0,contract_states,0,
	  [{clause,0,[],[],[erl_parse:abstract(StateNames, 0)]}]},
    Funcs =  [F1,F2,F3,F4,F5,F6,F7],
    Exports = {attribute,0,export,
	       [{contract_name,0},{contract_info,0},
		{contract_description,0},
		{contract_types,0},
		{contract_states,0},
		{contract_type,1},
		{contract_state,1}]},
    {Exports, Funcs}.

merge_in_code([H|T], Exports, Fns) when element(1, H) == function ->
    [Exports,H|Fns++T];
merge_in_code([H|T], Exports, Fns) ->
    [H|merge_in_code(T, Exports, Fns)];
merge_in_code([], Exports, Fns) ->
    [Exports|Fns].

%% usage 
%%    oil_parse:file(File)
%%        Converts File.ebnf -> File.xbin

make() ->
    make_lex(),
    make_yecc().

make_lex() -> leex:gen(contract, contract_lex),
	      c:c(contract_lex).

make_yecc() -> yecc:yecc("contract", "contract_yecc", true),
	       c:c(contract_yecc).

infileExtension()  -> ".con".
outfileExtension() -> ".buc".  %% binary UBF contract

batch([A]) ->    
    File = atom_to_list(A),
    Parse = file(File),
    io:format("~p~n", [Parse]),
    true.

file(F) ->
    case {infileExtension(), filename:extension(F)} of
	{X, X} -> 
	    io:format("Parsing ~s~n", [F]),
	    case file1(F) of
		{ok, Contract} ->
		    Enc = ubf:encode(Contract),
		    file:write_file(filename:rootname(F) ++ 
				    outfileExtension(),
				    Enc),
		    Size = length(Enc),
		    Bsize = size(term_to_binary(Contract)),
		    {ok, {ubfSize,Size,bsize,Bsize}};
		Error ->
		    Error
	    end;
	_ ->
	    {error, bad_extension}
    end.

file1(F) ->
    {ok, Stream} = file:open(F, read),
    P = handle(Stream, 1, [], 0),
    file:close(Stream),
    case P of
	{ok, P1} ->
	    case (catch pass2(F, P1)) of 
		{'EXIT', E} ->
		    {error, E};
		Contract ->
		    {ok, Contract}
	    end;
	E -> E
    end.

preDefinedTypes() -> [int, string, atom, binary, term, void].

pass2(F, P) ->
    Name = require(one, name, P),
    Vsn  = require(one, vsn, P),
    Info = require(one, info, P),
    Description = require(one, description, P),
    Types = require(many, type, P),
    Trans = require(many, transition, P),
    C = #contract{name=Name, vsn=Vsn, info=Info, description=
		  Description, types=Types, transitions=Trans},
    pass3(C).

require(Multiplicity, Tag, P) ->    
    Vals =  [ Val || {T,Val} <- P, T == Tag],
    case Multiplicity of
	one ->
	    case Vals of
		[V] -> V;
		_ -> 
		    io:format("~p missing or incorrectly defined~n",
			      [Tag]),
		    exit(parse)
	    end;
	many ->
	    Vals
    end.

pass3(C) ->
    Types = C#contract.types,
    Transitions = C#contract.transitions,
    Name = C#contract.name,
    Vsn = C#contract.vsn,
    DefinedTypes = map(fun({I,_}) -> I end, Types) ++ preDefinedTypes(),
    case duplicates(DefinedTypes, []) of
	[] -> true;
	L1 -> exit({duplicated_types, L1})
    end,
    UsedTypes = extract_prims({Types,Transitions}, []),
    MissingTypes = UsedTypes -- DefinedTypes,
    case MissingTypes of
	[] ->
	    DefinedStates = [S||{S,_} <- Transitions] ++ [stop],
	    %% io:format("defined states=~p~n",[DefinedStates]),
	    case duplicates(DefinedStates, []) of
		[] -> true;
		L2 -> exit({duplicated_states, L2})
	    end,
	    %% io:format("Transitions=~p~n",[Transitions]),
	    UsedStates0 = [S||{_,Rules} <- Transitions, 
			     {input,_,Out} <- Rules,
			     {output,_,S} <- Out],
	    UsedStates = remove_duplicates(UsedStates0),
	    %% io:format("Used States=~p~n",[UsedStates]),
	    MissingStates = filter(fun(I) -> 
					   not member(I, DefinedStates) end, 
				   UsedStates),
	    case MissingStates of 
		[] -> C;
		_  -> exit({missing_states, MissingStates})
	    end;
	_ ->
	    exit({missing_types, MissingTypes})
    end.
    
duplicates([H|T], L) ->
    case member(H, T) of
	true ->
	    duplicates(T, [H|L]);
	false ->
	    duplicates(T, L)
    end;
duplicates([], L) ->
    L.

extract_prims({prim, X}, L) -> 
    case member(X, L) of
	true  -> L;
	false -> [X|L]
    end;
extract_prims(T, L) when tuple(T) ->
    foldl(fun extract_prims/2, L, tuple_to_list(T));
extract_prims(T, L) when list(T) ->
    foldl(fun extract_prims/2, L, T);
extract_prims(T, L) ->
    L.

handle(Stream, LineNo, L, NErrors) ->
    handle1(io:requests(Stream, [{get_until,foo,contract_lex,
			  tokens,[LineNo]}]), Stream, L, NErrors).

handle1({ok, Toks, Next}, Stream, L, Nerrs) ->
    case contract_yecc:parse(Toks) of
	{ok, Parse} ->
	    %% io:format("Parse=~p~n",[Parse]),
	    handle(Stream, Next, [Parse|L], Nerrs);
	{error, {Line, Mod, What}} ->
	    Str = apply(Mod, format_error, [What]),
	    %% io:format("Toks=~p~n",[Toks]),
	    io:format("** ~w ~s~n", [Line, Str]),
	    %% handle(Stream, Next, L, Nerrs+1);
	    {error, 1};
	Other ->
	    io:format("Bad_parse:~p\n", [Other]),
	    handle(Stream, Next, L, Nerrs+1)
    end;
handle1({eof, _}, Stream, L, 0) ->
    {ok, lists:reverse(L)};
handle1({eof, _}, Stream, L, N) ->
    {error, N};
handle1(What, Stream, L, Nerrs) ->
    io:format("Here:~p\n", [What]),
    handle(Stream, 1, L, Nerrs+1).

remove_duplicates([H|T]) ->
    case member(H, T) of
	true ->
	    remove_duplicates(T);
	false ->
	    [H|remove_duplicates(T)]
    end;
remove_duplicates([]) -> [].







