equ
===

equ is a load balancer written in Erlang.

[![Build Status](https://travis-ci.org/polaris/equ.svg)](https://travis-ci.org/polaris/equ)


Building
--------

Run

    rebar compile

to compile equ.


Configuration
-------------

A configuration for a load balancer in front of two web servers could look like so

    [{backend_servers, [{"10.0.0.1", 80}, {"10.0.0.2", 80}]},
     {port, 80},
     {num_acceptors, 4}].

|=Parameter |=Description
|backend_servers |A list of backend server addresses and ports.
|port            |The port on which the load balancer listens for connections.
|num_acceptors   |The number of processes accepting incoming connections.


Controlling
-----------

Start equ with

    equ:start().

Stop equ with

    equ:stop().

Add backend servers at runtime with

    backend_server:add('1.0.0.3', 80).

Remove backend servers with

    backend_server:remove('1.0.0.3', 80).
