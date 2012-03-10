%%% The MIT License
%%%
%%% Copyright (C) 2011-2012 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Contract checking assistants for ABNF.

-module(contracts_abnf).

%%-compile(export_all).
-export([check_binary/4]).


-record(state,
        { check       % current contract type
          , x         % current binary to be checked
          , level     % current recursive level that has been checked so far
          , size      % binary size that has been checked so far
          , maxsize   % binary max size to be checked
          , mod       % contract
         }
       ).


%%----------------------------------------------------------------------
%% check_binary/4
%%
%% @doc Parse (and validate) the given binary against abnf contract
%% types.  It is straightforward to extend this implementation to
%% return the parsed abnf types stack for parsed (and invalid)
%% binaries.  The abnf types with sub-binary annotations could then be
%% used by other applications that need the abnf bits.  However, this
%% feature is beyond the current scope and goals of this package.
check_binary(Check, X, Level, Mod) ->
    S = #state{check=Check, x=X, level=Level, mod=Mod, maxsize=size(X), size=0},
    case check_binary(S) of
        {ok,_}=_Ok ->
            true;
        _Ng ->
            false
    end.


%% check_binary/1
check_binary(S) ->
    check_binary(S, fun all_checked/1).

all_checked(#state{check=undefined, size=MaxSize, maxsize=MaxSize}=S) ->
    {ok,S};
all_checked(S) ->
    {ng,S}.


%%----------------------------------------------------------------------
%% check_binary/2
check_binary(#state{check={abnf_alt,Types}}=S, Next) ->
    Funs = [ fun() -> check_binary(S#state{check=Type}, Next) end || Type <- Types ],
    alts(Funs);
check_binary(#state{check={abnf_seq,[Type|Types]}}=S, Next) ->
    Fun = fun(S1) -> check_binary(S1#state{check={abnf_seq, Types}}, Next) end,
    check_binary(S#state{check=Type}, Fun);
check_binary(#state{check={abnf_seq,[]}}=S, Next) ->
    Next(S#state{check=undefined});
check_binary(#state{check={abnf_repeat,_Min,_Max,_Type}}=S, Next) ->
    check_binary_repeat(S, Next, 0);
check_binary(#state{check={abnf_byte_range,Min,Max},x=X,size=Size}=S, Next) ->
    case X of
        <<Byte:8,Rest/binary>> ->
            if Min =< Byte andalso Byte =< Max ->
                    Next(S#state{check=undefined,x=Rest,size=Size+1});
               true ->
                    {ng,S}
            end;
        _ ->
            {ng,S}
    end;
check_binary(#state{check={abnf_byte_alt,Types}}=S, Next) ->
    Funs = [ fun() -> check_binary(S#state{check=Type}, Next) end || Type <- Types ],
    alts(Funs);
check_binary(#state{check={abnf_byte_seq,[Type|Types]}}=S, Next) ->
    Fun = fun(S1) -> check_binary(S1#state{check={abnf_byte_seq, Types}}, Next) end,
    check_binary(S#state{check=Type}, Fun);
check_binary(#state{check={abnf_byte_seq,[]}}=S, Next) ->
    Next(S#state{check=undefined});
check_binary(#state{check={abnf_byte_val,Byte},x=X,size=Size}=S, Next) ->
    case X of
        <<Byte:8,Rest/binary>> ->
            Next(S#state{check=undefined,x=Rest,size=Size+1});
        _ ->
            {ng,S}
    end;
check_binary(#state{check={prim,1,1,Type},level=Level,mod=Mod}=S, Next) ->
    %% NOTE: hard-coded max level of 10010
    if Level < 10010 ->
            case Type of
                {predef,_} ->
                    {ng,S};
                _ ->
                    TypeDef = element(1, Mod:contract_type(Type)),
                    check_binary(S#state{check=TypeDef,level=Level+1}, Next)
            end;
       true ->
            {ng,S}
    end;
check_binary(S, _Next) ->
    {ng,S}.


%%----------------------------------------------------------------------
%% check_binary_repeat
check_binary_repeat(#state{check={abnf_repeat,_Min,Max,_Type}=_Check}=S, Next, Matches)
  when Matches =:= Max ->
    Next(S#state{check=undefined});
check_binary_repeat(#state{check={abnf_repeat,Min,_Max,Type}=Check}=S, Next, Matches)
  when Matches < Min ->
    Fun = fun(S1) -> check_binary_repeat(S1#state{check=Check}, Next, Matches+1) end,
    check_binary(S#state{check=Type}, Fun);
check_binary_repeat(#state{check={abnf_repeat,_Min,Max,Type}=Check}=S, Next, Matches)
  when Max == infinity orelse Matches < Max ->
    Fun1 = fun() -> Next(S#state{check=undefined}) end,
    Fun2 = fun() -> Fun = fun(S1) -> check_binary_repeat(S1#state{check=Check}, Next, Matches+1) end,
                    check_binary(S#state{check=Type}, Fun) end,
    alts([Fun1,Fun2]);
check_binary_repeat(S, _Next, _Matches) ->
    {ng,S}.


%%----------------------------------------------------------------------
%% @doc Choose between alternatives (with backtracking).
%%
%% Based on ideas presented in
%% http://rvirding.blogspot.com/2009/03/backtracking-in-erlang-part-1-control.html[Robert
%% Virding\'s blog]

alts([C|Cs]) ->
    case C() of
        {ok,_Res}=Ok ->
            Ok;
        {ng,_}=Ng ->
            alts(Cs, Ng);
        ng ->
            alts(Cs)
    end;
alts([]) ->
    ng.

alts([C|Cs], Ng) ->
    case C() of
        {ok,_Res}=Ok ->
            Ok;
        {ng,_} ->
            alts(Cs, Ng);
        ng ->
            alts(Cs, Ng)
    end;
alts([], Ng) ->
    Ng.
