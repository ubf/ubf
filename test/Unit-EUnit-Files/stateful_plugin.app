%%% -*- mode: erlang -*-
%%%

{application, stateful_plugin,
 [
  {description, "STATEFUL_PLUGIN"},
  {vsn, "0.01"},
  {id, "STATEFUL_PLUGIN"},
  {modules, [
             %% TODO: fill in this list, perhaps
            ]
  },
  {registered, [ ] },
  %% NOTE: do not list applications which are load-only!
  {applications, [ kernel, stdlib, sasl ] },
  {mod, {stateful_plugin_app, []} }
 ]
}.
