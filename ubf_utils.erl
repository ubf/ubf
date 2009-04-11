-module(ubf_utils).

-export([debug/0, spawn_link_debug/2]).
-import(lists, [filter/2, foldl/3]).

spawn_link_debug(Term, Fun) ->
    spawn_link(fun() ->
		       put('$ubfinfo', Term),			     
		       Fun()
	       end).

debug() ->
    P = erlang:processes(),
    Live = filter(fun(I) -> is_process_alive(I) end, P),
    foldl(fun(I, A) ->
		  case process_info(I, dictionary) of
		      {dictionary, D} ->
			  case get_item('$ubfinfo', D) of
			      {yes, Info} ->
				  [Info|A];
			      no ->
				  A
			  end;
		      _ ->
			  A
		  end
	  end, [], Live).

get_item(K, [{K,V}|_]) -> {yes, V};
get_item(K, [_|T]) -> get_item(K, T);
get_item(K, []) -> no.
