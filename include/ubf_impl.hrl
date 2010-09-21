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

-record(contract,
        {name, vsn, types=[], leaftypenames=[], importtypenames=[], records=[], transitions=[], anystate=[]}).

-endif. % -ifndef(ubf_impl)
