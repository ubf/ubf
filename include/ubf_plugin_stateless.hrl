%%%----------------------------------------------------------------------
%%% Description: UBF Plugin Stateless Utilities
%%%----------------------------------------------------------------------

-ifndef(ubf_plugin_stateless).
-define(ubf_plugin_stateless, true).

%%%-------------------------------------------------------------------
%%% Specs/Types
%%%-------------------------------------------------------------------

%% common callback API
-spec info() -> string().
-spec description() -> string().
-spec handlerStop(Handler::pid(), Reason::term(), StateData::term()) ->
                  NewStateData::term().

%% stateless callback API
-spec handlerStart(Args::term()) ->
                  {accept, Reply::term(), StateName::atom(), StateDate::term()} |
                  {reject, Reply::term()}.
-spec handlerRpc(Call::term()) -> Reply::term().

-endif. % -ifndef(ubf_plugin_stateless)
