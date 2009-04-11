-module(bug).

-compile(export_all).

test() ->
    match([[123,[]]]).

match( [ [H1,H2|T] | Stack]) ->
    {ok, H1, H2, T, Stack};
match(C) ->
    nomatch.
