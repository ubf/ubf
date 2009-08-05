%%% -*- mode: erlang -*-
%%%

{application, test,
 [
  {description, "TEST"},
  {vsn, "0.01"},
  {id, "TEST"},
  {modules, [
             %% TODO: fill in this list, perhaps
            ]
  },
  {registered, [ ] },
  %% NOTE: do not list applications which are load-only!
  {applications, [ kernel, stdlib, sasl ] },
  {mod, {test_app, []} }
 ]
}.
