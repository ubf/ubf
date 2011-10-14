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

%%% @doc Utilities for UBF
%%%-------------------------------------------------------------------

-module(ubf_utils).
-include("ubf.hrl").

-export([ubf_contract/1, ubf_contract/2]).

get_type(Name,Mod) ->
    get_type(Name,true,Mod).

get_type(Name,Strict,Mod) ->
    case lists:member(Name,Mod:contract_types()) of
        true ->
            {Type, Tag} = Mod:contract_type(Name),
            {Name, Type, Tag};
        false ->
            if Strict ->
                    exit({undefined_ubf_type,Name});
               true ->
                    undefined
            end
    end.

ubf(Name,Mod) ->
    {Name,Type,_} = get_type(Name,Mod),
    type(Type,Mod).

type(Type,Mod) ->
    io_lib:format("\t\t~s\n", [typeref(Type,Mod)]).

%% alt
typeref({alt,Type1,Type2},Mod) ->
    io_lib:format("~s | ~s", [typeref(Type1,Mod), typeref(Type2,Mod)]);
%% prim
typeref({prim,Min,Max,Tag},Mod) ->
    case Tag of
        {predef,_} ->
            PrimTag = typeref(Tag, Mod),
            case {Min,Max} of
                {1,1} ->
                    io_lib:format("~s", [PrimTag]);
                {0,1} ->
                    io_lib:format("~s?", [PrimTag]);
                {0,0} ->
                    io_lib:format("~s{0}", [PrimTag])
            end;
        _ ->
            case {Min,Max} of
                {1,1} ->
                    io_lib:format("~p()", [Tag]);
                {0,1} ->
                    io_lib:format("~p()?", [Tag]);
                {0,0} ->
                    io_lib:format("~p(){0}", [Tag])
            end
    end;
%% tuple
typeref({tuple,Elements},Mod) ->
    io_lib:format("{ ~s }", [join([typeref(Element,Mod) || Element <- Elements], ", ")]);
%% record
typeref({record,RecName,Elements},Mod) when is_atom(RecName) ->
    Values = tl(tl(Elements)),
    RecordKey = {RecName,length(Elements)-2},
    Fields = Mod:contract_record(RecordKey),
    io_lib:format("#~s{ ~s }",
                  [RecName, join([ io_lib:format("~s=~s", [Field, typeref(Element,Mod)])
                                   || {Field,Element} <- lists:zip(Fields,Values) ], ", ")]);
typeref({record_ext,RecName,_,_Elements},_Mod) when is_atom(RecName) ->
    erlang:exit(fatal);
%% list
typeref({list,0,infinity,Element},Mod) ->
    io_lib:format("[~s]", [typeref(Element,Mod)]);
typeref({list,0,1,Element},Mod) ->
    io_lib:format("[~s]?", [typeref(Element,Mod)]);
typeref({list,1,infinity,Element},Mod) ->
    io_lib:format("[~s]+", [typeref(Element,Mod)]);
typeref({list,M,M,Element},Mod) ->
    io_lib:format("[~s]{~p}", [typeref(Element,Mod), M]);
typeref({list,M,infinity,Element},Mod) ->
    io_lib:format("[~s]{~p,}", [typeref(Element,Mod), M]);
typeref({list,M,N,Element},Mod) ->
    io_lib:format("[~s]{~p,~p}", [typeref(Element,Mod), M, N]);
%% range
typeref({range,infinity,Hi},_Mod) ->
    io_lib:format("..~p", [Hi]);
typeref({range,Lo,infinity},_Mod) ->
    io_lib:format("~p..", [Lo]);
typeref({range,Lo,Hi},_Mod) ->
    io_lib:format("~p..~p", [Lo, Hi]);
%% atom
typeref({atom,Value},_Mod) ->
    io_lib:format("~p", [Value]);
%% binary
typeref({binary,Value},_Mod) ->
    io_lib:format("~p", [Value]);
%% float
typeref({float,Value},_Mod) ->
    io_lib:format("~p", [Value]);
%% integer
typeref({integer,Value},_Mod) ->
    io_lib:format("~p", [Value]);
%% string
typeref({string,Value},_Mod) ->
    io_lib:format("~p", [Value]);
%% predef
typeref({predef,atom},_Mod) ->
    "atom()";
typeref({predef,binary},_Mod) ->
    "binary()";
typeref({predef,float},_Mod) ->
    "float()";
typeref({predef,integer},_Mod) ->
    "integer()";
typeref({predef,list},_Mod) ->
    "list()";
typeref({predef,proplist},_Mod) ->
    "proplist()";
typeref({predef,string},_Mod) ->
    "string()";
typeref({predef,term},_Mod) ->
    "term()";
typeref({predef,tuple},_Mod) ->
    "tuple()";
typeref({predef,void},_Mod) ->
    erlang:exit(fatal);
%% predef with attributes
typeref({predef,{atom,Attrs}},_Mod) ->
    io_lib:format("atom(~s)", [join([ atom_to_list(Attr) || Attr <- Attrs ], ",")]);
typeref({predef,{binary,Attrs}},_Mod) ->
    io_lib:format("binary(~s)", [join([ atom_to_list(Attr) || Attr <- Attrs ], ",")]);
typeref({predef,{list,Attrs}},_Mod) ->
    io_lib:format("list(~s)", [join([ atom_to_list(Attr) || Attr <- Attrs ], ",")]);
typeref({predef,{proplist,Attrs}},_Mod) ->
    io_lib:format("proplist(~s)", [join([ atom_to_list(Attr) || Attr <- Attrs ], ",")]);
typeref({predef,{string,Attrs}},_Mod) ->
    io_lib:format("string(~s)", [join([ atom_to_list(Attr) || Attr <- Attrs ], ",")]);
typeref({predef,{term,Attrs}},_Mod) ->
    io_lib:format("term(~s)", [join([ atom_to_list(Attr) || Attr <- Attrs ], ",")]);
typeref({predef,{tuple,Attrs}},_Mod) ->
    io_lib:format("tuple(~s)", [join([ atom_to_list(Attr) || Attr <- Attrs ], ",")]);
%% otherwise
typeref(Type, _Mod) ->
    io_lib:format("~p()", [Type]).

ubf_contract(Mod, FileName) ->
    Contract = ubf_contract(Mod),
    file:write_file(FileName, Contract).

ubf_contract(Mod) when is_list(Mod) ->
    ubf_contract(list_to_atom(Mod));
ubf_contract(Mod) ->
    X0 = [""
          , "%%%"
          , "%%% Auto-generated by ubf_utils:ubf_contract()"
          , "%%% Do not edit manually!"
          , "%%%"
          , ""
          , ""
         ],
    X1 = ["%% --------------------"
          , "%% pre defined types"
          , "%%   - left hand-side is UBF"
          , "%%   - right hand-side is Erlang-like"
          , "%%   - A() means replace with \"A type reference\""
          , "%%   - A() | B() means \"A() or B()\""
          , "%%   - A()? means \"optional A()\""
          , "%%   - A(Attrs) means \"A() subject to the comma-delimited type attributes"
          , "%%"
          , ""
          , "true\n\t\ttrue"
          , "false\n\t\tfalse"
          , "undefined\n\t\tundefined"
          , ""
          , "atom()\n\t\tatom"
          , "atom()?\n\t\tatom | undefined"
          , ""
          , "binary()\n\t\tbinary"
          , "binary()?\n\t\tbinary | undefined"
          , ""
          , "float()\n\t\tfloat"
          , "float()?\n\t\tfloat | undefined"
          , ""
          , "integer()\n\t\tinteger"
          , "integer()?\n\t\tinteger | undefined"
          , ""
          , "list()\n\t\tlist"
          , "list()?\n\t\tlist | undefined"
          , ""
          , "proplist()\n\t\t{'#P',proplist}"
          , "proplist()?\n\t\t{'#P',proplist} | undefined"
          , ""
          , "string()\n\t\t{'#S',string}"
          , "string()?\n\t\t{'#S',string} | undefined"
          , ""
          , "term()\n\t\tterm"
          , "term()?\n\t\tterm | undefined"
          , ""
          , "tuple()\n\t\ttuple"
          , "tuple()?\n\t\ttuple | undefined"
          , ""
          , "void()\n\t\t /* no result is returned */"
          , "void()?\n\t\t /* no result is returned */ | undefined"
          , ""
          , "%% --------------------"
          , "%% type attributes"
          , "%%"
          , ""
          , "atom(AtomAttrs)\n\t\tatom"
          , "atom(AtomAttrs)?\n\t\tatom | undefined"
          , ""
          , "binary(BinaryAttrs)\n\t\tbinary"
          , "binary(BinaryAttrs)?\n\t\tbinary | undefined"
          , ""
          , "list(ListAttrs)\n\t\tlist"
          , "list(ListAttrs)?\n\t\tlist | undefined"
          , ""
          , "proplist(PropListAttrs)\n\t\t{'#P',proplist}"
          , "proplist(PropListAttrs)?\n\t\t{'#P',proplist} | undefined"
          , ""
          , "string(StringAttrs)\n\t\t{'#S',string}"
          , "string(StringAttrs)?\n\t\t{'#S',string} | undefined"
          , ""
          , "tuple(TupleAttrs)\n\t\ttuple"
          , "tuple(TupleAttrs)?\n\t\ttuple | undefined"
          , ""
          , "term(TermAttrs)\n\t\tterm"
          , "term(TermAttrs)?\n\t\tterm | undefined"
          , ""
          , "AtomAttrs"
          , "\t ascii | asciiprintable"
          , "\t nonempty"
          , "\t nonundefined"
          , ""
          , "BinaryAttrs"
          , "\t ascii | asciiprintable"
          , "\t nonempty"
          , ""
          , "ListAttrs"
          , "\t nonempty"
          , ""
          , "PropListAttrs"
          , "\t nonempty"
          , ""
          , "StringAttrs"
          , "\t ascii | asciiprintable"
          , "\t nonempty"
          , ""
          , "TermAttrs"
          , "\t nonempty"
          , "\t nonundefined"
          , ""
          , "TupleAttrs"
          , "\t nonempty"
          , ""
          , "%% --------------------"
          , "%% user-defined attributes"
          , "%%"
          , ""
          , "{type1() ...}\n\t\ttuple"
          , "{type1() ...}?\n\t\ttuple | undefined"
          , ""
          , "record()\n\t\trecord"
          , "record()?\n\t\trecord | undefined"
          , ""
          , "[type()]\n\t\tlist of type()"
          , "[type()]?\n\t\tlist of length 0 or length 1 of type()"
          , "[type()]+\n\t\tlist of length greater than 0 of type()"
          , "[type()]{M}\n\t\tlist of length M of type()"
          , "[type()]{M,}\n\t\tlist of minimum length M of type()"
          , "[type()]{M,N}\n\t\tlist of minimum length M and maximum length N of type()"
          , ""
          , "integer()..integer()\n\t\tinteger"
          , "..integer()\n\t\tinteger"
          , "integer()..\n\t\tinteger"
          , ""
          , "%% --------------------"
          , "%% leaf types"
          , "%%"
          , ""
          , ""
         ],
    X2 = [ [atom_to_list(Name), "()", "\n", ubf(Name,Mod)]
           || Name <- lists:sort(Mod:contract_leaftypes()) ],
    X3 = [""
          , "%% --------------------"
          , "%% UBF-RPC"
          , "%%"
          , ""
         ],
    X4 = [ begin
               {Input, InputType, _} = get_type(Input,true,Mod),
               Params = typeref(InputType,Mod),
               Result =
                   case get_type(Output,false,Mod) of
                       {Output, OutputType, _} ->
                           typeref(OutputType,Mod);
                       undefined ->
                           io_lib:format("~p()", [Output])
                   end,
               join([
                     ""
                     , "%% ----------"
                     , io_lib:format("%% ~s", [Input])
                     , "%%"
                     , io_lib:format("~s =>", [Params])
                     , io_lib:format("   ~s;", [Result])
                    ], "\n")
           end
           || {{prim,1,1,Input}, {prim,1,1,Output}} <- Mod:contract_anystate() ],
    X5 = ["\n"],
    lists:flatten([ join(L, "\n") || L <- [X0, X1, X2, X3, X4, X5] ]).

join(L, Sep) ->
    lists:flatten(join2(L, Sep)).

join2([A, B|Rest], Sep) ->
    [A, Sep|join2([B|Rest], Sep)];
join2(L, _Sep) ->
    L.
