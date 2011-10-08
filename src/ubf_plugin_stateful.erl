%% @doc Callbacks for stateful plugin implementations.
%%
%% <img src="ubf-flow-01.png"></img>

-module(ubf_plugin_stateful).

%% Interface Functions
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{info,0}
     , {description,0}
     , {handlerStart,2}
     , {handlerStop,3}
     , {handlerRpc,4}
    ].

