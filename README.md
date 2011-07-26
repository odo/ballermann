ballermann
=====

ballermann (German for tender shepherd) is a tool for load balancing between Erlang processes in a round-robin fashion.

Building
--------

get and install rebar: https://github.com/basho/rebar

```
git clone git://github.com/odo/ballermann.git
cd ballermann
rebar compile
```

Usage
--------

The idea is to balance between processes owned by a supervisor. In this example we use the sasl supervisor (```sasl_sup```) which has two sub-processes. In a typical setting, you would balance between a large set of identical gen_server processes.

```
1> application:start(sasl).
ok
2> ballermann_sup:start_link(sasl_sup, sasl_pool).
{ok,<0.43.0>}
3> ballermann:pid(sasl_pool).
<0.40.0>
4> ballermann:pid(sasl_pool).
<0.37.0>
5> ballermann:pid(sasl_pool).
<0.40.0>
6> ballermann:pid(sasl_pool).
<0.37.0>
```

If you want to get funky, you can specify when the pool is refreshed, meaning when the supervisor is asked for it's children.
The first parameter is the number of calls before a refresh is forced (default 10000), the second the number of milliseconds a refresh takes place (default 5000).

```
ballermann_sup:start_link(sasl_sup, sasl_pool2, 1000, 500).
```
