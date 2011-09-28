Ballermann
=====

Ballermann (German for "the one who drinks from the spring, gently") is a tool for load balancing between Erlang processes in a round-robin fashion.

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
2> ballermann:balance(sasl_sup, sasl_pool).
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
You do so by specifying a ratio. If the number of alive processes in the pool in relation to the original number drops below this ratio, ballermann turns to the supervisor and asks for new processes.
The default is 0.8 (80 %).

```
ballermann:balance(sasl_sup, sasl_pool2, 0.9).
```

Sometimes, when the processes behind ballermann have side effects, you want ballermann to stop handing out pids and perform some cleanup. This can be achieved using apply_within(ServerName, {Module, Function, Args, WaitTimeOrFun}). When you provide a numeric value, ballermann will wait for that number of milliseconds. If you provide a fun with arity 1, ballermann will call that function with each pid in the pool as an argument:

```
1> application:start(sasl).
ok
2> ballermann:balance(sasl_sup, sasl_pool).
{ok,<0.43.0>}
3> ballermann:apply_within(sasl_pool, {lists, reverse, [[1, 2]]}, 1000).
[2,1]
4> ballermann:apply_within(sasl_pool, {lists, reverse, [[1, 2]]}, fun(P) -> P end).
[2,1]
```

If you have a pool of gen_servers running and you want to make sure that they are all idle before you do your maintenance, the call would more likely look like this:

```
ballermann:apply_within(server_pool, {my_cleanup_module, cleanup, [now]}, fun(Pid) -> gen_server:call(Pid, {ping}) end).
```
 


Tests
--------

to test ballermann, uncomment the line

```{eunit_compile_opts, [{d, 'BALLERMANNTEST'}]}.```

in rebar.config and run

```rebar eunit```

This is to prevent ballermann from been compiled in test mode when used as a dependency (which is the most common situation).