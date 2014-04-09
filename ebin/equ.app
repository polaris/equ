%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*- %%
{application, equ,
  [{description, "A simple TCP load balancer"},
   {vsn, "0.1.0"},
   {modules, [equ,
              equ_app, 
              equ_sup,
              backend_server,
              acceptor_sup,
              acceptor_server,
              proxy_server]},
  {env, []},
  {registered, [equ, equ_sup]},
  {applications, [kernel, stdlib]},
  {mod, {equ_app, []}}
]}.