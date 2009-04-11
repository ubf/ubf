-module(mods).

-export([mods/0]).
-import(lists, [filter/2, map/2, member/2]).

mods() ->
    C = code:all_loaded(),
    New = map(fun({Mod,_}) -> Mod end, C),
    case get('$loaded_mods') of
	undefined ->
	    put('$loaded_mods', New),
	    [];
	Old ->
	    Added = filter(fun(I) -> not member(I, Old) end, New),
	    Missing = missing(Added),
	    put('$loaded_mods', New),
	    Missing
    end.

missing(Mods) ->
    Lib = code:lib_dir(),
    {ok, This} = file:get_cwd(),
    T = map(fun(I) -> locate(I, Lib, This) end, Mods),
    [M || {other, M} <- T].
		   
locate(Mod, Lib, This) ->
    case code:is_loaded(Mod) of
	{file, F} ->
	    D = filename:dirname(F),
	    case prefix(Lib, D) of
		true ->
		    library;
		false ->
		    case prefix(This, D) of
			true ->
			    local;
			false ->
			    {other, F}
		    end
	    end;
	_ ->
	    {other, Mod}
    end.

prefix([H|T], [H|T1]) -> prefix(T, T1);
prefix([], _)         -> true;
prefix(_, _)          -> false.

    
    

