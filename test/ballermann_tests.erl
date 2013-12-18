-module(ballermann_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ballermann_test_() ->
    [{foreach, local,
      fun test_setup/0,
      fun test_teardown/1,
      [
        fun test_balance/0,
        fun test_apply_within/0,
        fun test_no_supervisor_init/0,
        fun test_no_children_init/0,
        fun test_no_children/0,
        fun test_no_supervisor/0,
        fun test_exit/0
      ]}
    ].

test_setup() ->
    application:start(sasl),
    meck:new(ballermann, [unstick, passthrough]).

test_teardown(_) ->
    meck:unload(ballermann).

test_balance() ->
    meck:expect(ballermann, child_pids, fun(_) -> [1, 2, 3] end),
    {ok, StateInit} = ballermann:init({supervisor, 0.75}),
    {reply, Pid1, State1} = ballermann:handle_call({pid}, x, StateInit),
    ?assertEqual(1, Pid1),
    {reply, Pid2, State2} = ballermann:handle_call({pid}, x, State1),
    ?assertEqual(2, Pid2),
    {reply, Pid3, State3} = ballermann:handle_call({pid}, x, State2),
    ?assertEqual(3, Pid3),
    {reply, Pid4, _State4} = ballermann:handle_call({pid}, x, State3),
    ?assertEqual(1, Pid4).

test_apply_within() ->
    meck:expect(ballermann, child_pids, fun(_) -> [1, 2, 3] end),
    {ok, StateInit} = ballermann:init({supervisor, 0.75}),
    {reply, Reply, StateInit} = ballermann:handle_call({apply_within, {lists, reverse, [[1, 2]]}, 0}, x, StateInit),
    ?assertEqual([2, 1], Reply),
    {reply, Reply2, StateInit} = ballermann:handle_call({apply_within, {lists, reverse, [[2, 1]]}, 100}, x, StateInit),
    ?assertEqual([1, 2], Reply2),
    {reply, Reply3, StateInit} = ballermann:handle_call({apply_within, {lists, reverse, [[2, 3]]}, fun(P) -> P end}, x, StateInit),
    ?assertEqual([3, 2], Reply3).

test_no_supervisor_init() ->
    Error =
    try
        ballermann:init({supervisor, 0.75})
    catch
        exit:Reason -> Reason
    end,
    ?assertEqual({error, supervisor_not_running}, Error).

test_no_children_init() ->
    meck:expect(ballermann, child_pids, fun(_) -> [] end),
    Error =
    try
        ballermann:init({supervisor, 0.75})
    catch
        exit:Reason -> Reason
    end,
    ?assertEqual({error, supervisor_has_no_children}, Error).

test_no_children() ->
    meck:expect(ballermann, child_pids, fun(_) -> [1, 2] end),
    {ok, StateInit} = ballermann:init({supervisor, 0.75}),
    meck:expect(ballermann, child_pids, fun(_) -> [] end),
    Error =
    try
        {noreply, StateDown} =
        ballermann:handle_info({'DOWN', x, x, 1, x}, StateInit),
        ballermann:handle_info({'DOWN', x, x, 2, x}, StateDown)
    catch
        exit:Reason -> Reason
    end,
    ?assertEqual({error, supervisor_has_no_children}, Error).

test_no_supervisor() ->
    meck:expect(ballermann, child_pids, fun(_) -> [1, 2] end),
    {ok, StateInit} = ballermann:init({supervisor, 0.75}),
    meck:expect(ballermann, child_pids, fun(Arg) -> ballermann_meck_original:child_pids(Arg) end),
    Error =
    try
        ballermann:handle_info({'DOWN', x, x, 1, x}, StateInit)
    catch
        exit:Reason -> Reason
    end,
    ?assertEqual({error, supervisor_not_running}, Error).

test_exit() ->
    meck:expect(ballermann, child_pids, fun(_) -> [1,2,3] end),
    {ok, StateInit} = ballermann:init({supervisor, 0.75}),
    meck:expect(ballermann, child_pids, fun(_) -> [1,3] end),
    {noreply, StateDown} = ballermann:handle_info({'DOWN', x, x, 2, x}, StateInit),
    {reply, Pid1, State1} = ballermann:handle_call({pid}, x, StateDown),
    ?assertEqual(1, Pid1),
    {reply, Pid2, State2} = ballermann:handle_call({pid}, x, State1),
    ?assertEqual(3, Pid2),
    {reply, Pid3, State3} = ballermann:handle_call({pid}, x, State2),
    ?assertEqual(1, Pid3),
    meck:expect(ballermann, child_pids, fun(_) -> [1] end),
    {noreply, StateDown2} = ballermann:handle_info({'DOWN', x, x, 3, x}, State3),
    {reply, Pid4, State4} = ballermann:handle_call({pid}, x, StateDown2),
    ?assertEqual(1, Pid4),
    {reply, Pid5, _State5} = ballermann:handle_call({pid}, x, State4),
    ?assertEqual(1, Pid5).

-endif.

