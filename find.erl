-module(find).

%% Module:  find.erl
%% Author:  Joe Armstrong (joe.armstrong@telia.com)
%% Date:    2001-10-17
%% Purpose: Find all files. Find all out of date files
%%          Finds all files relative to a given root directory. 
%%
%% Examples:
%%   find:files(".", "*.erl", false)
%%     finds all files in the current directory.
%%     Recursive scan of sub-directories is also allowed.
%%
%%   out_of_date checks for "out of date files".
%%     find:out_of_date(".",".erl",".jam")
%%     finds all out of date Erlang files in the current directory.
%%
%%   find:files(Dir, RegExp, Recursive, Fun/2, Acc0)
%%      applies Fun(File, Acc) -> Acc. to each file

-export([files/3, files/5, out_of_date/3]).

-import(lists, [suffix/2, sublist/3, map/2, filter/2, reverse/1]).
-import(misc, [is_file/1, outofdate/2, writeable/1]).

%% find(Dir, ReExpr, Recursive) -> [File]
%%     Find regular files starting from Dir
%%     Which match ReExpr
%%     If Recursive is true do recursivly on all sub-directories
%%     Example find(".", "*.erl", false) will find all erlang files in the 
%%     Current directory
%%
%% out_of_date(Dir, SrcExt, ObjExt) find all "out of date files" in
%%     Dir.
%%     Example:
%%         out_of_date(".", ".erl", ".jam") 
%%             Finds all out of date files in the current directory

%%+type files(string(), string(), bool()) -> [string()].

files(Dir, Reg, Recursive, Fun, Acc) ->
    case file:list_dir(Dir) of
	{ok, Files} -> find_files(Files, Dir, Reg, Recursive, Fun, Acc);
	{error, _}  -> Acc
    end.

find_files([File|T], Dir, Reg, Recursive, Fun, Acc0) ->
    FullName = Dir ++  [$/|File],
    case file_type(FullName) of
	regular ->
	    case regexp:match(FullName, Reg) of
		{match, _, _}  -> 
		    Acc = Fun(FullName, Acc0),
		    find_files(T, Dir, Reg, Recursive, Fun, Acc);
		_ ->
		    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
	    end;
	directory -> 
	    case Recursive of
		true ->
		    Acc1 = files(FullName, Reg, Recursive, Fun, Acc0),
		    find_files(T, Dir, Reg, Recursive, Fun, Acc1);
		false ->
		    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
	    end;
	error -> 
	    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
    end;
find_files([], _, _, _, _, A) ->
    A.

%% compatibility with earlier stuff
files(Dir, Re, Flag) -> 
    Re1 = regexp:sh_to_awk(Re),
    reverse(files(Dir, Re1, Flag, fun(File, Acc) ->[File|Acc] end, [])).

-include_lib("kernel/include/file.hrl").

file_type(File) ->
    case file:read_file_info(File) of
	{ok, Facts} ->
	    case Facts#file_info.type of
		regular   -> regular;
		directory -> directory;
		_         -> error
	    end;
	_ ->
	    error
    end.


%%______________________________________________________________________
%% outofdate(Dir, InExtension, OutExtension)
%%   scans Dir for all files with the extension "InExtension"
%%   If a file with this extension is found then "OutExtension" is checked
%%
%%   returns a list of files in <Dir> where *.OutExtension is
%%   "out of date" with respect to *.InExtension
%%   in the sence of "make"

out_of_date(Dir, In, Out) ->
    case file:list_dir(Dir) of
	{ok, Files0} ->
	    Files1 = filter(fun(F) -> suffix(In, F) end, Files0),
	    Files2 = map(fun(F) -> sublist(F, 1, length(F) - length(In)) end, 
			 Files1),
	    filter(fun(F) -> update(F, In, Out) end, Files2);
	_ ->
	    []
    end.
	  
update(File, In, Out) ->
    InFile  = File ++ In,
    OutFile = File ++ Out,
    case is_file(OutFile) of
	true ->
	    case writeable(OutFile) of
		true ->
		    outofdate(InFile, OutFile);
		false ->
		    %% can't write so we can't update
		    false
	    end;
	false ->
	    %% doesn't exist
	    true
    end.




