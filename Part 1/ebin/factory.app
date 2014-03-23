%%-*- mode: erlang -*-

{application, factory,
 [
  {description, ""},
  {vsn, ""},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {modules, [
             cc, cc_storage, factory, factory_storage, session,
             session_storage, sf
            ]}
 ]}.
