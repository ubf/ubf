-module(contracts_abnf).

%%-compile(export_all).
-export([check_binary/5]).


-define(FAIL(_X), false).


%% check_binary
check_binary({abnf_alt, [Type|Types]}=_Check, X, Level, Mod, Size) ->
    %% @todo first match is not always desired
    case check_binary(Type, X, Level, Mod, Size) of
        NewSize when is_integer(NewSize) ->
            NewSize;
        _ ->
            check_binary({abnf_alt, Types}, X, Level, Mod, Size)
    end;
check_binary({abnf_alt, []}=_Check, _X, _Level, _Mod, _Size) ->
    false;
check_binary({abnf_seq, [Type|Types]}=_Check, X, Level, Mod, Size) ->
    case check_binary(Type, X, Level, Mod, Size) of
        NewSize when is_integer(NewSize) ->
            DeltaSize = 8*(NewSize - Size),
            <<_Bytes:DeltaSize, Rest/binary>> = X,
            check_binary({abnf_seq, Types}, Rest, Level, Mod, NewSize);
        _ ->
            false
    end;
check_binary({abnf_seq, []}=_Check, _, _Level, _Mod, Size) ->
    Size;
check_binary({abnf_repeat,Min,Max,Type}=_Check, X, Level, Mod, Size) ->
    check_binary_repeat(Min, Max, Type, X, Level, Mod, Size, 0);
check_binary({abnf_byte_range, Min, Max}=_Check, X, _Level, _Mod, Size) ->
    case X of
        <<Byte:8, _/binary>> ->
            if Min =< Byte andalso Byte =< Max ->
                    Size+1;
               true ->
                    false
            end;
        _ ->
            false
    end;
check_binary({abnf_byte_alt, [Type|Types]}=_Check, X, Level, Mod, Size) ->
    %% @todo first match is not always desired
    case check_binary(Type, X, Level, Mod, Size) of
        NewSize when is_integer(NewSize) ->
            NewSize;
        _ ->
            check_binary({abnf_byte_alt, Types}, X, Level, Mod, Size)
    end;
check_binary({abnf_byte_alt, []}=_Check, _X, _Level, _Mod, _Size) ->
    false;
check_binary({abnf_byte_seq, [Type|Types]}=_Check, X, Level, Mod, Size) ->
    case check_binary(Type, X, Level, Mod, Size) of
        NewSize when is_integer(NewSize) ->
            <<_Byte:8, Rest/binary>> = X,
            check_binary({abnf_byte_seq, Types}, Rest, Level, Mod, NewSize);
        _ ->
            false
    end;
check_binary({abnf_byte_seq, []}=_Check, _, _Level, _Mod, Size) ->
    Size;
check_binary({abnf_byte_val, Byte}=_Check, X, _Level, _Mod, Size) ->
    case X of
        <<Byte:8, _/binary>> ->
            Size+1;
        _ ->
            false
    end;
check_binary({prim, 1, 1, Type}=_Check, X, Level, Mod, Size) ->
    %% NOTE: hard-coded max level of 10010
    if Level < 10010 ->
            case Type of
                {predef,_} ->
                    false;
                _ ->
                    TypeDef = element(1, Mod:contract_type(Type)),
                    check_binary(TypeDef, X, Level+1, Mod, Size)
            end;
       true ->
            ?FAIL({maxlevel,X})
    end;
%% otherwise, fail
check_binary(_Check, _X, _Level, _Mod, _Size) ->
    %% io:format("~p isnot ~p~n", [Check, X]),
    %% exit({Y,isNotA, X}).
    false.


%% check_binary_repeat
check_binary_repeat(0, 0, _Type, _X, _Level, _Mod, Size, _Matches) ->
    Size;
check_binary_repeat(Min, Max, Type, X, Level, Mod, Size, Matches) ->
    case check_binary(Type, X, Level, Mod, Size) of
        NewSize when is_integer(NewSize) ->
            if (Max /= infinity andalso Matches+1 >= Max) ->
                    NewSize;
               true ->
                    DeltaSize = 8*(NewSize - Size),
                    <<_Byte:DeltaSize, Rest/binary>> = X,
                    check_binary_repeat(Min, Max, Type, Rest, Level, Mod, NewSize, Matches+1)
            end;
        _ when Matches >= Min ->
            Size;
        _ ->
            false
    end.
