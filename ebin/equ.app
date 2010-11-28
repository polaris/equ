%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*- %%
{application, equ,
  [{description, "A simple TCP load balancer"},
   {vsn, "0.1.0"},
   {modules, [equ_app, 
              equ_sup,
              equ_server,
              proxy]},
  {registered, [equ_sup]},
  {applications, [kernel, stdlib]},
  {mod, {equ_app, []}}
]}.