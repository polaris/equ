equ
===

equ is a load balancer written in Erlang.

[![Build Status](https://travis-ci.org/polaris/equ.svg)](https://travis-ci.org/polaris/equ)


Build
-----

Run

    rebar compile

to compile equ.


Configure
---------

equ tries to read equ.config when it starts.

Parameter | Description | Default
--- | --- | ---
backend_servers | A list of backend server addresses and ports. | []
port | The port on which the load balancer listens for connections. | 2307
num_acceptors | The number of processes accepting incoming connections. | 4
proxy_timeout | The time a proxy process stays alive without any client interaction. | 60000

A configuration for a load balancer in front of two web servers could look like so

    [{backend_servers, [{"10.0.0.1", 80}, {"10.0.0.2", 80}]},
     {port, 80},
     {num_acceptors, 4},
     {proxy_timeout, 60000}].


Run
---

Start equ

    equ:start().

Stop equ

    equ:stop().

Add backend servers at runtime to equ

    backend_list:add('1.0.0.3', 80).

Remove backend servers at runtime from equ

    backend_list:remove('1.0.0.3', 80).
