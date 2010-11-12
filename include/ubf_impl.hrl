%%%----------------------------------------------------------------------
%%% Description: UBF Implementation Utilities
%%%----------------------------------------------------------------------

-ifndef(ubf_impl).
-define(ubf_impl, true).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------

-define(UBF_TLOG_MODULE_DEFAULT, undefined).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------

-type contract_name() :: nonempty_string().
-type contract_vsn() :: nonempty_string().
-type contract_types() :: [term()].
-type contract_typenames() :: [atom()].
-type contract_records() :: [term()].
-type contract_transitions() :: [term()].
-type contract_anystate() :: [term()].

-record(contract, {
          name :: contract_name(),
          vsn :: contract_vsn(),
          types=[] :: contract_types(),
          leaftypenames=[] :: contract_typenames(),
          importtypenames=[] :: contract_typenames(),
          records=[] :: contract_records(),
          transitions=[] :: contract_transitions(),
          anystate=[] :: contract_anystate()
         }).

-endif. % -ifndef(ubf_impl)
