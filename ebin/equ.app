%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*- %%
{application, equ,
  [{description, "A simple TCP load balancer"},
   {vsn, "0.1.0"},
   {modules, [equ,
              equ_app, 
              equ_sup,
              equ_server,
              backend_server,
              acceptor_sup,
              acceptor_server,
              proxy_server]},
  {env, [{backend_servers, [{"127.0.0.1", 1235}, {"127.0.0.1", 1236}]}]},
  {registered, [equ, equ_sup]},
  {applications, [kernel, stdlib]},
  {mod, {equ_app, []}}
]}.