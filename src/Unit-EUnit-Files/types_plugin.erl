%%% Description: ubf test types plugin
%%%----------------------------------------------------------------------

%%% @doc UBF Test Types Plugin.
%%%
%%% @end
%%%

-module(types_plugin).

%% NOTE the following two lines
-compile({parse_transform,contract_parser}).
-add_contract("./Unit-EUnit-Files/types_plugin").
