%%% The MIT License
%%%
%%% Copyright (C) 2011-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc UBF(b) contract parser.
%%%
%%% Parsing a UBF(b) contract is done via a compiler "parse transform"
%%% during the usual compilation of an Erlang source module.
%%%
%%% @TODO More documentation required here.

-module(contract_parser).

%%% parse contract language
%%% Copyright 2002 Joe Armstrong (joe@sics.se)
%%% Documentation http:://www.sics.se/~joe/ubf.html

-include("ubf_impl.hrl").

-export([predefined_types/0, predefined_types/1,
         builtin_types/0, builtin_types/1,
         parse_transform/2, parse_transform/5,
         parse_transform_contract/2,
         parse_stream/3,
         tags/1, tags/2
        ]).

%%====================================================================
%% External API
%%====================================================================

predefined_types() ->
    predefined_types(withoutattrs) ++ predefined_types(withattrs).

predefined_types(withoutattrs) ->
    [any, none, atom, binary, float, integer, list, tuple];
predefined_types(withattrs) ->
    [
     %% any
     {any,[nonempty]}, {any,[nonundefined]}
     , {any,[nonempty,nonundefined]}
     %% atom
     , {atom,[ascii]}, {atom,[asciiprintable]}, {atom,[nonempty]}, {atom,[nonundefined]}
     , {atom,[ascii,nonempty]}, {atom,[ascii,nonundefined]}, {atom,[asciiprintable,nonempty]}, {atom,[asciiprintable,nonundefined]}
     , {atom,[ascii,nonempty,nonundefined]}, {atom,[asciiprintable,nonempty,nonundefined]}
     , {atom,[nonempty,nonundefined]}
     %% binary
     , {binary,[ascii]}, {binary,[asciiprintable]}, {binary,[nonempty]}
     , {binary,[ascii,nonempty]}, {binary,[asciiprintable,nonempty]}
     %% list
     , {list,[nonempty]}
     %% tuple
     , {tuple,[nonempty]}
    ].

builtin_types() ->
    builtin_types(erlang) ++ builtin_types(ubf).

builtin_types(erlang) ->
    [nil, term, boolean, byte, char, non_neg_integer, pos_integer, neg_integer, number, string, nonempty_string, module, mfa, node, timeout, no_return];
builtin_types(ubf) ->
    [ubfproplist, ubfstring].

parse_transform(In, Opts) ->
    Imports = [X||{attribute,_,add_types,X} <- In],
    case [X||{attribute,_,add_contract,X} <- In] of
        [File] ->
            {Out, _Contract} = parse_transform(In, Opts, File ++ infileExtension(), Imports, fun parse_file/1, false),
            Out;
        [] ->
            %% add (hacky) support for parameterized modules
            case [X||{attribute,_,add_pmod_contract,X} <- In] of
                [File] ->
                    {Out, _Contract} = parse_transform(In, Opts, File ++ infileExtension(), Imports, fun parse_file/1, true),
                    Out;
                [] ->
                    In
            end
    end.

parse_transform(In, Opts, File, Imports, ParseFun) ->
    parse_transform(In, Opts, File, Imports, ParseFun, false).

parse_transform(In, Opts, File, Imports, ParseFun, IsPmod) ->
    case file(File, Imports, ParseFun) of
        {ok, Contract} ->
            HeaderFile =
                filename:join(
                  case proplists:get_value(outdir, Opts) of undefined -> "."; OutDir -> OutDir end
                  , filename:rootname(filename:basename(File)) ++ outfileHRLExtension()
                 ),
            ok = ubf_utils:ubf_contract(Contract, HeaderFile),
            {parse_transform_contract(In, Contract, IsPmod), Contract};
        {error, Reason} ->
            io:format("Error in contract:~p~n", [Reason]),
            erlang:error(Reason)
    end.

parse_transform_contract(In, Contract) ->
    parse_transform_contract(In, Contract, false).

parse_transform_contract(In, Contract, IsPmod) ->
    {Export, Fns} = make_code(Contract, IsPmod),
    In1 = merge_in_code(In, Export, Fns),
    In1.

parse_stream(Stream, Lex, Yecc) ->
    parse_stream(Stream, Lex, Yecc, 1, [], 0).

tags(P1) ->
    tags(P1, []).

tags(P1, Imports) ->
    case (catch pass2(P1, Imports)) of
        {'EXIT', E} ->
            {error, E};
        Contract ->
            case (catch pass4(Contract)) of
                {'EXIT', E} ->
                    {error, E};
                Records ->
                    case pass5(Contract) of
                        [] ->
                            noop;
                        UnusedTypes ->
                            if Contract#contract.transitions =/= [] orelse Contract#contract.anystate =/= [] ->
                                    case UnusedTypes -- Contract#contract.importtypenames of
                                        [] ->
                                            noop;
                                        UnusedNonImportTypes ->
                                            erlang:error({unused_types, UnusedNonImportTypes})
                                    end;
                               true ->
                                    noop
                            end
                    end,
                    %% extra leaf type names
                    LeafTypeNames = pass6(Contract),
                    {ok, Contract#contract{records=Records, leaftypenames=LeafTypeNames}}
            end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

infileExtension()  -> ".con".
outfileHRLExtension() -> ".hrl".  %% hrl UBF contract records

make_code(C, false) ->
    make_code1(C, []);
make_code(C, true) ->
    make_code1(C, [{var,0,'_'}]).

make_code1(C, Args) ->
    ArgsLen = length(Args),
    %% contract name
    F1 = {function,0,contract_name,ArgsLen,
          [{clause,0,Args,[],[{string,0,C#contract.name}]}]},
    %% contract vsn
    F2 = {function,0,contract_vsn,ArgsLen,
          [{clause,0,Args,[],[{string,0,C#contract.vsn}]}]},
    %% contract types
    TypeNames = lists:map(fun({Type,_, _}) -> Type end, C#contract.types),
    F3 = {function,0,contract_types,ArgsLen,
          [{clause,0,Args,[],[erl_parse:abstract(TypeNames, 0)]}]},
    %% contract leaftypes
    LeafTypeNames = C#contract.leaftypenames,
    F4 = {function,0,contract_leaftypes,ArgsLen,
          [{clause,0,Args,[],[erl_parse:abstract(LeafTypeNames, 0)]}]},
    %% contract importtypes
    ImportTypeNames = C#contract.importtypenames,
    F5 = {function,0,contract_importtypes,ArgsLen,
          [{clause,0,Args,[],[erl_parse:abstract(ImportTypeNames, 0)]}]},
    %% contract records
    RecordNames = lists:map(fun({Record, _}) -> Record end, C#contract.records),
    F6 = {function,0,contract_records,ArgsLen,
          [{clause,0,Args,[],[erl_parse:abstract(RecordNames, 0)]}]},
    %% contract states
    StateNames =
        if C#contract.transitions =:= [] ->
                [];
           true ->
                lists:map(fun({State,_}) -> State end, C#contract.transitions)
        end,
    F7 = {function,0,contract_states,ArgsLen,
          [{clause,0,Args,[],[erl_parse:abstract(StateNames, 0)]}]},
    %% contract type
    Type =
        if C#contract.types =:= [] ->
                [{clause,1,[{var,0,'_'}|Args],[],[erl_parse:abstract([], 0)]}];
           true ->
                lists:map(fun({Type,Val,Str}) ->
                                  {clause,1,[{atom,0,Type}|Args],[],[erl_parse:abstract({Val,Str})]}
                          end, C#contract.types)
        end,
    F8 = {function,0,contract_type,ArgsLen+1,Type},
    %% contract record
    Record =
        if C#contract.records =:= [] ->
                [{clause,1,[{var,0,'_'}|Args],[],[erl_parse:abstract([], 0)]}];
           true ->
                lists:map(fun({Record, Val}) ->
                                  {clause,1,[{atom,0,Record}|Args],[],[erl_parse:abstract(Val)]}
                          end, C#contract.records)
        end,
    F9 = {function,0,contract_record,ArgsLen+1,Record},
    %% contract state
    State =
        if C#contract.transitions =:= [] ->
                [{clause,1,[{var,0,'_'}|Args],[],[erl_parse:abstract([], 0)]}];
           true ->
                lists:map(fun({State,Val}) ->
                                  {clause,1,[{atom,0,State}|Args],[],[erl_parse:abstract(Val)]}
                          end, C#contract.transitions)
        end,
    F10 = {function,0,contract_state,ArgsLen+1,State},
    %% contract anystate
    Any =
        if C#contract.anystate =:= [] ->
                [];
           true ->
                C#contract.anystate
        end,
    F11 = {function,0,contract_anystate,ArgsLen,
           [{clause,0,Args,[],[erl_parse:abstract(Any, 0)]}]},
    %% exports
    Exports = {attribute,0,export,
               [{contract_name,ArgsLen},
                {contract_vsn,ArgsLen},
                {contract_types,ArgsLen},
                {contract_leaftypes,ArgsLen},
                {contract_importtypes,ArgsLen},
                {contract_records,ArgsLen},
                {contract_states,ArgsLen},
                {contract_type,ArgsLen+1},
                {contract_record,ArgsLen+1},
                {contract_state,ArgsLen+1},
                {contract_anystate,ArgsLen}
               ]},
    %% funcs
    Funcs = [F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11],
    {Exports, Funcs}.

merge_in_code([H|T], Exports, Fns)
  when element(1, H) == function orelse element(1, H) == eof ->
    [Exports,H|Fns++T];
merge_in_code([H|T], Exports, Fns) ->
    [H|merge_in_code(T, Exports, Fns)];
merge_in_code([], Exports, Fns) ->
    [Exports|Fns].

parse_file(F) ->
    {ok, Stream} = file:open(F, [read]),
    try
        parse_stream(Stream, contract_lex, contract_yecc)
    after
        ok = file:close(Stream)
    end.

parse_stream(Stream, Lex, Yecc, LineNo, L, NErrors) ->
    Requests = [{get_until,foo,Lex,tokens,[LineNo]}],
    parse_stream1(io:requests(Stream, Requests), Stream, Lex, Yecc, L, NErrors).

parse_stream1({ok, Toks, Next}, Stream, Lex, Yecc, L, Nerrs) ->
    case Yecc:parse(Toks) of
        {ok, Parse} ->
            parse_stream(Stream, Lex, Yecc, Next, [Parse|L], Nerrs);
        {error, {Line, Mod, What}} ->
            Str = Mod:format_error(What),
            io:format("** ~w ~s~n", [Line, Str]),
            {error, 1};
        Other ->
            io:format("Bad_parse:~p\n", [Other]),
            parse_stream(Stream, Lex, Yecc, Next, L, Nerrs+1)
    end;
parse_stream1({eof, _}, _Stream, _Lex, _Yecc, L, 0) ->
    {ok, lists:reverse(L)};
parse_stream1({eof, _}, _Stream, _Lex, _Yecc, _L, N) ->
    {error, N};
parse_stream1(What, _Stream, _Lex, _Yecc, _L, _N) ->
    {error, What}.

file(F, Imports, ParseFun) ->
    case ParseFun(F) of
        {ok, P} ->
            tags(P, Imports);
        E ->
            E
    end.

pass2(P, Imports) ->
    Name = require(one, name, P),
    Vsn = require(one, vsn, P),
    Types = require(zero_or_one, types, P),
    Any = require(zero_or_one, anystate, P),
    Trans = require(many, transition, P),
    AllImports = if Name =/= "ubf_types_builtin" -> [ubf_types_builtin|Imports]; true -> Imports end,

    ImportTypes = lists:flatten(
                    [
                     begin
                         case Import of
                             Mod when is_atom(Mod) ->
                                 Mod = Mod,
                                 TL = Mod:contract_types();
                             {Mod, TL} when is_atom(Mod), is_list(TL) ->
                                 Mod = Mod,
                                 TL = TL;
                             {Mod, {except, ETL}} when is_atom(Mod), is_list(ETL) ->
                                 Mod = Mod,
                                 TL = Mod:contract_types() -- ETL;
                             X ->
                                 Mod = unused,
                                 TL = unused,
                                 exit({invalid_import, X})
                         end,
                         [ begin {TDef, TTag} = Mod:contract_type(T), {T, TDef, TTag} end
                           || T <- TL ]
                     end
                     || Import <- AllImports ]
                   ),
    ImportTypeNames = [ T || {T, _, _} <- ImportTypes ],

    C = #contract{name=Name, vsn=Vsn, anystate=lists:sort(Any),
                  types=lists:sort(Types),
                  importtypenames=lists:sort(ImportTypeNames),
                  transitions=lists:sort(Trans)},
    pass3(C, ImportTypes).

require(Multiplicity, Tag, P) ->
    Vals = [ Val || {T,Val} <- P, T == Tag ],
    case Multiplicity of
        zero_or_one ->
            case Vals of
                [] -> [];
                [V] -> V;
                _ ->
                    io:format("~p incorrectly defined~n",
                              [Tag]),
                    erlang:error(parse)
            end;
        one ->
            case Vals of
                [V] -> V;
                _ ->
                    io:format("~p missing or incorrectly defined~n",
                              [Tag]),
                    erlang:error(parse)
            end;
        many ->
            Vals
    end.

pass3(C1, ImportTypes) ->
    Types1 = C1#contract.types,
    Transitions = C1#contract.transitions,
    _Name = C1#contract.name,
    _Vsn = C1#contract.vsn,
    AnyState = C1#contract.anystate,

    DefinedTypes1 = [ I || {I,_,_} <- Types1 ] ++ [ {predef, I} || I <- predefined_types() ],
    case duplicates(DefinedTypes1, []) of
        [] -> true;
        L1 -> erlang:error({duplicated_types, L1})
    end,

    C2 = C1#contract{types=lists:usort(Types1 ++ ImportTypes)},
    Types2 = C2#contract.types,
    DefinedTypes2 = [ I || {I,_,_} <- Types2 ] ++ [ {predef, I} || I <- predefined_types() ],
    case duplicates(DefinedTypes2, []) of
        [] -> true;
        L2 -> erlang:error({duplicated_unmatched_import_types, L2})
    end,

    UsedTypes = extract_prims({Types2,Transitions,AnyState}, []),
    MissingTypes = UsedTypes -- DefinedTypes2,
    case MissingTypes of
        [] ->
            DefinedStates = [S||{S,_} <- Transitions] ++ [stop],
            case duplicates(DefinedStates, []) of
                [] -> true;
                L3 -> erlang:error({duplicated_states, L3})
            end,
            UsedStates0 = [S||{_,Rules} <- Transitions,
                              {input,_,Out} <- Rules,
                              {output,_,S} <- Out],
            UsedStates = remove_duplicates(UsedStates0),
            MissingStates = lists:filter(fun(I) -> not lists:member(I, DefinedStates) end,
                                         UsedStates),
            case MissingStates of
                [] -> C2;
                _  -> erlang:error({missing_states, MissingStates})
            end;
        _ ->
            erlang:error({missing_types, MissingTypes})
    end.

pass4(C) ->
    Types = C#contract.types,
    Records = extract_all_records(Types,[]),
    case duplicates(Records, []) of
        [] -> true;
        L1 -> erlang:error({duplicated_records, L1})
    end,
    %% filter records
    lists:sort([ {{Name,length(FDTs)+1}, FDTs } || {Name,FDTs} <- Records ]).

pass5(C) ->
    Transitions = C#contract.transitions,
    AnyState = C#contract.anystate,
    UsedTypes = extract_prims({Transitions,AnyState}, []),
    pass5(C, UsedTypes, []).

pass5(C, [], L) ->
    Types = C#contract.types,
    DefinedTypes = lists:map(fun({I,_, _}) -> I end, Types),
    DefinedTypes -- L;
pass5(C, [H|T], L) ->
    Types = C#contract.types,
    TypeDef = [ Y || {X,Y,_Z} <- Types, X =:= H ],
    UsedTypes = [ UsedType || UsedType <- extract_prims(TypeDef, []), not lists:member(UsedType, L) ],
    pass5(C, T ++ UsedTypes, [H|L]).

pass6(C) ->
    Transitions = C#contract.transitions,
    AnyState = C#contract.anystate,
    RootUsedTypes = extract_prims({Transitions,AnyState}, []),
    pass6(C, RootUsedTypes, RootUsedTypes, []).

pass6(C, RootUsedTypes, [], L) ->
    Types = C#contract.types,
    DefinedTypes = lists:map(fun({I,_, _}) -> I end, Types),
    lists:sort(DefinedTypes -- (RootUsedTypes -- L));
pass6(C, RootUsedTypes, [H|T], L) ->
    Types = C#contract.types,
    TypeDef = [ Y || {X,Y,_Z} <- Types, X =:= H ],
    UsedTypes = [ UsedType || UsedType <- extract_prims(TypeDef, []), lists:member(UsedType, RootUsedTypes) ],
    pass6(C, RootUsedTypes, T, UsedTypes ++ L).

duplicates([H|T], L) ->
    case lists:member(H, T) of
        true ->
            duplicates(T, [H|L]);
        false ->
            duplicates(T, L)
    end;
duplicates([], L) ->
    L.

extract_prims({prim, _Min, _Max, X}, L) ->
    case lists:member(X, L) of
        false -> [X|L];
        true  -> L
    end;
extract_prims(T, L) when is_tuple(T) ->
    lists:foldl(fun extract_prims/2, L, tuple_to_list(T));
extract_prims(T, L) when is_list(T) ->
    lists:foldl(fun extract_prims/2, L, T);
extract_prims(_T, L) ->
    L.

%% ignore nested records
extract_all_records({Tag, Name, Fields, Defaults, Types}, L)
  when Tag=:=record;
       Tag=:=record_ext ->
    X = {Name,lists:zip3(Fields,Defaults,tl(tuple_to_list(Types)))},
    case lists:member(X, L) of
        true  -> L;
        false -> [X|L]
    end;
extract_all_records(T, L) when is_tuple(T) ->
    lists:foldl(fun extract_all_records/2, L, tuple_to_list(T));
extract_all_records(T, L) when is_list(T) ->
    lists:foldl(fun extract_all_records/2, L, T);
extract_all_records(_T, L) ->
    L.

remove_duplicates([H|T]) ->
    case lists:member(H, T) of
        true ->
            remove_duplicates(T);
        false ->
            [H|remove_duplicates(T)]
    end;
remove_duplicates([]) -> [].
