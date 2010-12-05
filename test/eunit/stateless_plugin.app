%%% -*- mode: erlang -*-
%%%

{application, stateless_plugin,
 [
  {description, "STATELESS_PLUGIN"},
  {vsn, "0.01"},
  {id, "STATELESS_PLUGIN"},
  {modules, [
             %% TODO: fill in this list, perhaps
            ]
  },
  {registered, [ ] },
  %% NOTE: do not list applications which are load-only!
  {applications, [ kernel, stdlib, sasl ] },
  {mod, {stateless_plugin_app, []} }
 ]
}.
