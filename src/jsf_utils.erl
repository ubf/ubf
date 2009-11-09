%%% $Id$
%%% @doc Low-level utilities for JSF.
%%%-------------------------------------------------------------------

-module(jsf_utils).
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
%% concat
typeref({concat,Type1,Type2},Mod) ->
    io_lib:format("~s ++ ~s", [typeref(Type1,Mod), typeref(Type2,Mod)]);
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
    io_lib:format("{\"$T\" : [ ~s ]}", [join([typeref(Element,Mod) || Element <- Elements], ", ")]);
%% record
typeref({record,RecName,Elements},Mod) when is_atom(RecName) ->
    Values = tl(tl(Elements)),
    RecordKey = {RecName,length(Elements)-2},
    Fields = Mod:contract_record(RecordKey),
    io_lib:format("{\"$R\" : \"~p\", ~s}",
                  [RecName, join([ io_lib:format("\"~p\" : ~s", [Field, typeref(Element,Mod)])
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
typeref({atom,true},_Mod) ->
    "true";
typeref({atom,false},_Mod) ->
    "false";
typeref({atom,undefined},_Mod) ->
    "null";
typeref({atom,Value},_Mod) ->
    io_lib:format("{\"$A\" : \"~p\"}", [Value]);
%% binary
typeref({binary,Value},_Mod) ->
    io_lib:format("\"~p\"", [Value]);
%% float
typeref({float,Value},_Mod) ->
    io_lib:format("~p", [Value]);
%% integer
typeref({integer,Value},_Mod) ->
    io_lib:format("~p", [Value]);
%% string
typeref({string,Value},_Mod) ->
    io_lib:format("{\"$S\" : \"~p\"}", [Value]);
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
          , "///"
          , "/// Auto-generated by jsf_utils:ubf_contract()"
          , "/// Do not edit manually!"
          , "///"
          , ""
          , ""
         ],
    X1 = ["// --------------------"
          , "// pre defined types"
          , "//   - left hand-side is UBF"
          , "//   - right hand-side is JSON"
          , "//   - A() means replace with \"A type reference\""
          , "//   - A() | B() means \"A() or B()\""
          , "//   - A()? means \"optional A()\""
          , "//   - A() ++ B() means \"list A() concatenate list B()"
          , "//   - A(Attrs) means \"A() subject to the comma-delimited type attributes"
          , "//"
          , ""
          , "true\n\t\ttrue"
          , "false\n\t\tfalse"
          , "undefined\n\t\tnull"
          , ""
          , "atom()\n\t\t{\"$A\" : string }"
          , "atom()?\n\t\t{\"$A\" : string } | null"
          , ""
          , "binary()\n\t\tstring"
          , "binary()?\n\t\tstring | null"
          , ""
          , "float()\n\t\tint frac"
          , "float()?\n\t\tint frac | null"
          , ""
          , "integer()\n\t\tint"
          , "integer()?\n\t\tint | null"
          , ""
          , "list()\n\t\tarray"
          , "list()?\n\t\tarray | null"
          , ""
          , "proplist()\n\t\tobject"
          , "proplist()?\n\t\tobject | undefined"
          , ""
          , "string()\n\t\t{\"$S\" : string }"
          , "string()?\n\t\t{\"$S\" : string } | null"
          , ""
          , "term()\n\t\tvalue"
          , "term()?\n\t\tvalue | null"
          , ""
          , "tuple()\n\t\t{\"$T\" : array }"
          , "tuple()?\n\t\t{\"$T\" : array } | null"
          , ""
          , "void()\n\t\t /* no result is returned */"
          , "void()?\n\t\t /* no result is returned */ | null"
          , ""
          , "// --------------------"
          , "// type attributes"
          , "//"
          , ""
          , "atom(AtomAttrs)\n\t\t{\"$A\" : string }"
          , "atom(AtomAttrs)?\n\t\t{\"$A\" : string } | null"
          , ""
          , "binary(BinaryAttrs)\n\t\tstring"
          , "binary(BinaryAttrs)?\n\t\tstring | null"
          , ""
          , "list(ListAttrs)\n\t\tarray"
          , "list(ListAttrs)?\n\t\tarray | null"
          , ""
          , "proplist(PropListAttrs)\n\t\tobject"
          , "proplist(PropListAttrs)?\n\t\tobject | undefined"
          , ""
          , "string(StringAttrs)\n\t\t{\"$S\" : string }"
          , "string(StringAttrs)?\n\t\t{\"$S\" : string } | null"
          , ""
          , "tuple(TupleAttrs)\n\t\t{\"$T\" : array }"
          , "tuple(TupleAttrs)?\n\t\t{\"$T\" : array } | null"
          , ""
          , "term(TermAttrs)\n\t\tvalue"
          , "term(TermAttrs)?\n\t\tvalue | null"
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
          , "// --------------------"
          , "// user-defined attributes"
          , "//"
          , ""
          , "{type1() ...}\n\t\t{\"$T\" : array }"
          , "{type1() ...}?\n\t\t{\"$T\" : array } | null"
          , ""
          , "record()\n\t\t{\"$R\" : string, pair ... }"
          , "record()?\n\t\t{\"$R\" : string, pair ... } | null"
          , ""
          , "[type()]\n\t\tarray of type()"
          , "[type()]?\n\t\tarray of length 0 or length 1 of type()"
          , "[type()]+\n\t\tarray of length greater than 0 of type()"
          , "[type()]{0}\n\t\tempty array"
          , "[type()]{M}\n\t\tarray of length M of type()"
          , "[type()]{M,}\n\t\tarray of minimum length M of type()"
          , "[type()]{M,N}\n\t\tarray of minimum length M and maximum length N of type()"
          , ""
          , "integer()..integer()\n\t\tint "
          , "..integer()\n\t\tint"
          , "integer()..\n\t\tint"
          , ""
          , "// --------------------"
          , "// leaf types"
          , "//"
          , ""
          , ""
         ],
    X2 = [ [atom_to_list(Name), "()", "\n", ubf(Name,Mod)]
           || Name <- lists:sort(Mod:contract_leaftypes()) ],
    X3 = [""
          , "// --------------------"
          , "// JSON-RPC"
          , "//"
          , ""
         ],
    X4 = [ begin
               {InputTag, Params} =
                   case get_type(Input,true,Mod) of
                       {_Input, {tuple, [{atom, Atom}|Elements1]}, _} ->
                           {Atom
                            , io_lib:format("[ ~s ]", [join([typeref(E,Mod) || E <- Elements1, E =/= {prim,1,1,authinfo}], ", ")])
                           };
                       {_Input, {atom, Atom}, _} ->
                           {Atom
                            , io_lib:format("[]"
                                            , [])
                           }
                   end,
               Result =
                   case get_type(Output,false,Mod) of
                       {Output, OutputType, _} ->
                           typeref(OutputType,Mod);
                       undefined ->
                           io_lib:format("~p()", [Output])
                   end,
               join([
                     ""
                     , "// ----------"
                     , io_lib:format("// ~p", [InputTag])
                     , "//"
                     , "request {"
                     , io_lib:format("\t\"version\" : \"1.1\",", [])
                     , io_lib:format("\t\"id\"      : binary(),", [])
                     , io_lib:format("\t\"method\"  : \"~p\",", [InputTag])
                     , io_lib:format("\t\"params\"  : ~s", [Params])
                     , " }"
                     , "response {"
                     , io_lib:format("\t\"version\" : \"1.1\",", [])
                     , io_lib:format("\t\"id\"      : binary(),", [])
                     , io_lib:format("\t\"result\"  : ~s | null,", [Result])
                     , io_lib:format("\t\"error\"   : error()?", [])
                     , " }"
                    ], "\n")
           end
           || {{prim,1,1,Input}, {prim,1,1,Output}} <- Mod:contract_anystate() ],

    X5 = [""
          , ""
          , "error() = null"
          , "\t| { \"name\" : \"JSONRPCError\", \"code\" : 100, \"message\" : \"Parse error (clientBrokeRPC)\", \"error\" : term()? }"
          , "\t| { \"name\" : \"JSONRPCError\", \"code\" : 101, \"message\" : \"Procedure not found (clientBrokeRPC)\", \"error\" : term()? }"

          , "\t| { \"name\" : \"JSONRPCError\", \"code\" : 102, \"message\" : \"Bad call (clientBrokeContract)\", \"error\" : term()? }"
          , "\t| { \"name\" : \"JSONRPCError\", \"code\" : 103, \"message\" : \"Service error (serverBrokeContract)\", \"error\" : term()? }"
          , ""
         ],
    X6 =
        [""
         , ""
         , "// ----------"
         , "// HTTP"
         , "//"
         , "// query parameters:"
         , "//"
         , "// \t\"x_max_content_length\" requests the server to enforce a max response body size"
         , "//"
         , "// request headers:"
         , "//"
         , "// \t\"X-Auth-Source-IP-Address\" requests the server to use the specified source ip address for client authorization"
         , "//"
         , "// response status code and reason phrases:"
         , "//"
         , "// \t\"513 Message Too Large\" informs the client the max response body size was exceeded"
         , "//"
         , ""
        ],

    lists:flatten([ join(L, "\n") || L <- [X0, X1, X2, X3, X4, X5, X6] ]).

join(L, Sep) ->
    lists:flatten(join2(L, Sep)).

join2([A, B|Rest], Sep) ->
    [A, Sep|join2([B|Rest], Sep)];
join2(L, _Sep) ->
    L.
