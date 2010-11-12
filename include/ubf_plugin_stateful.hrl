%%%----------------------------------------------------------------------
%%% Description: UBF Plugin Stateful Utilities
%%%----------------------------------------------------------------------

-ifndef(ubf_plugin_stateful).
-define(ubf_plugin_stateful, true).

%%%-------------------------------------------------------------------
%%% Specs/Types
%%%-------------------------------------------------------------------

%% common callback API
-spec info() -> string().
-spec description() -> string().
-spec handlerStop(Handler::pid(), Reason::term(), StateData::term()) ->
                  NewStateData::term().

%% stateful callback API
-spec handlerStart(Args::term(), Manager::pid()) ->
                  {accept, Reply::term(), StateName::atom(), StateDate::term()} |
                  {reject, Reply::term()}.
-spec handlerRpc(StateName::atom(), Call::term(), StateDate::term(), Manager::pid()) ->
                {Reply::term(), NewStateName::atom(), NewStateData::term()}.

-spec managerStart(Args::term()) ->
                   {ok, ManagerData::term()}.
-spec managerRestart(Args::term(), Manager::pid()) ->
                     ok | {error, Reason::term()}.
-spec managerRpc(Args::term(), ManagerData::term()) ->
                 {ok, NewManagerData::term()} | {error, Reason::term()}.

-endif. % -ifndef(ubf_plugin_stateful)
