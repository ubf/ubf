%% @doc Callbacks for stateless plugin implementations.
%%
%% <img src="../priv/doc/ubf-flow-01.png"></img>

-module(ubf_plugin_stateless).

%% Interface Functions
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{info,0}
     , {description,0}
     , {handlerStart,1}
     , {handlerStop,3}
     , {handlerRpc,1}
    ].

