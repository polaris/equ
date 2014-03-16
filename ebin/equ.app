%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*- %%
{application, equ,
  [{description, "A simple TCP load balancer"},
   {vsn, "0.1.0"},
   {modules, [equ_app, 
              equ_sup,
              equ_server,
              backend_server,
              proxy]},
  {env, [{backend_servers, [{"192.168.2.140", 81}, {"192.168.2.140", 80}]}]},
  {registered, [equ_sup]},
  {applications, [kernel, stdlib]},
  {mod, {equ_app, []}}
]}.