-module (ballermann).

-ifdef(BALLERMANNTEST).
	-include_lib("eunit/include/eunit.hrl").
	-define(supervisor_which_children(Supervisor), which_children_mock()).
	-define(whereis(Supervisor), whereis_mock()).
	-define(monitor(Type, Pid), monitor).
	-define(is_pid(Pid), is_integer(Pid)).
-else.
	-define(supervisor_which_children(Supervisor), supervisor:which_children(Supervisor)).
	-define(whereis(Supervisor), ?MODULE:whereis(Supervisor)).
	-define(monitor(Type, Pid), monitor(Type, Pid)).
	-define(is_pid(Pid), is_pid(Pid)).
-endif.

-behaviour (gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([balance/2, balance/3, apply_within/2, apply_within/3, start_link/3, pid/1, whereis/1]).
-type sup_ref()  :: {atom(), atom()}.

-record(state, {
	supervisor :: sup_ref(),
	pid_table :: atom(),
	last_pid :: pid(),
	pids_count_original :: integer(),
	min_alive_ratio :: float()
	}).

start_link(Supervisor, ServerName, MinAliveRatio) ->
	gen_server:start_link({local, ServerName}, ?MODULE, {Supervisor, MinAliveRatio}, []).

balance(Supervisor, BalancerName) ->
	ballermann_sup:start_link(Supervisor, BalancerName).

balance(Supervisor, BalancerName, MinAliveRatio) ->
	ballermann_sup:start_link(Supervisor, BalancerName, MinAliveRatio).

pid(ServerName) ->
	gen_server:call(ServerName, {pid}).

apply_within(ServerName, {Module, Function, Args}) ->
	apply_within(ServerName, {Module, Function, Args}, 0).

apply_within(ServerName, {Module, Function, Args}, WaitTimeOrFun) ->
	gen_server:call(ServerName, {apply_within, {Module, Function, Args}, WaitTimeOrFun}).

init({Supervisor, MinAliveRatio}) ->
	PidTableName = ets:new(pid_table, [private, duplicate_bag]),
	add_missing_pids(PidTableName, Supervisor),
	State = #state{
		supervisor 					= Supervisor,
		pids_count_original = table_size(PidTableName),
		min_alive_ratio 		= MinAliveRatio,
		pid_table 					= PidTableName,
		last_pid 						= ets:first(PidTableName)},
	{ok, State}.

handle_call({pid}, _From, State = #state{last_pid = LastPid, pid_table = PidTable}) ->
	Pid = case ets:next(PidTable, LastPid) of
		Value when ?is_pid(Value) ->
			Value;
		'$end_of_table' ->
			ets:first(PidTable)
	end,
	{reply, Pid, State#state{last_pid = Pid}};

handle_call({apply_within, {Module, Function, Args}, WaitTimeOrFun}, _From, State = #state{supervisor = Supervisor}) ->
	case WaitTimeOrFun of
		WaitTime when is_float(WaitTime) orelse is_integer(WaitTime) ->
			timer:sleep(WaitTime);
		Fun when is_function(Fun) ->
			[Fun(Pid) || Pid <- child_pids(Supervisor)]
	end,
	Reply = apply(Module, Function, Args),
	{reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({'DOWN', _, _, Pid, _}, State = #state{supervisor = Supervisor, last_pid = LastPid, pid_table = PidTable, pids_count_original = PidsCountOriginal, min_alive_ratio = MinAliveRatio}) ->
	error_logger:info_msg("~p: The process ~p (child of ~p) died.\n", [?MODULE, Pid, Supervisor]),
	ets:delete(PidTable, Pid),
	case too_few_pids(PidTable, PidsCountOriginal, MinAliveRatio) of
		true ->
			error_logger:warning_msg("~p: Reloading children from supervisor ~p.\n", [?MODULE, Supervisor]),
			add_missing_pids(PidTable, Supervisor);
		false ->
			ignore
	end,
	% Pick a valid LastPid, the recent one might be the one which just died.
	LastPidSave = case LastPid of
		Pid ->
			ets:first(PidTable);
		_ ->
			LastPid
	end,
	{noreply, State#state{last_pid = LastPidSave}}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

check_zero_pids(PidTable, Supervisor) ->
	case table_size(PidTable) of
		0 ->
			error_logger:error_msg("~p: Supervisor ~p has no children. Giving up.\n", [?MODULE, Supervisor]),
			exit({error, supervisor_has_no_children});
		_ -> ignore
	end.

too_few_pids(PidTable, PidsCountOriginal, MinAliveRatio) ->
	table_size(PidTable) / PidsCountOriginal < MinAliveRatio.

add_missing_pids(Table, Supervisor) ->
	Pids = child_pids(Supervisor),
	PidsNew = lists:filter(fun(E) -> ets:lookup(Table, E) =:= [] end, Pids),
	error_logger:info_msg("~p: Found ~p new processes of ~p total.\n", [?MODULE, length(PidsNew), length(Pids)]),
	PidsWithRefs = [{Pid, {?monitor(process, Pid)}}|| Pid <- PidsNew],
	ets:insert(Table, PidsWithRefs),
	check_zero_pids(Table, Supervisor).

child_pids(Supervisor) ->
	case ?whereis(Supervisor) of
		undefined ->
			error_logger:error_msg("~p Supervisor ~p not running. Giving up.\n", [?MODULE, Supervisor]),
			exit({error, supervisor_not_running});
		_ ->
				[ Pid || {_, Pid, _, _} <- ?supervisor_which_children(Supervisor)]
	end.

whereis(Supervisor) when is_pid(Supervisor) ->
    case erlang:is_process_alive(Supervisor) of
        true  -> Supervisor;
        false -> undefined
    end;

whereis(Supervisor) ->
    ?MODULE:whereis(erlang:whereis(Supervisor)).

table_size(Table) ->
		{size, Count} = proplists:lookup(size, ets:info(Table)),
		Count.

% tests ###############################

-ifdef(BALLERMANNTEST).

% this is called instead of supervisor:which_children/1 in tests
which_children_mock() ->
	[{x, Pid, x, x} || Pid <- erlang:get(children_mock)].

% this is called instead of whereis/1 in tests
whereis_mock() ->
	erlang:get(whereis_mock).

mock_test() ->
	erlang:put(children_mock, [pid_mock]),
	?assertEqual([{x, pid_mock, x, x}], ?supervisor_which_children(supervisor)),
	erlang:put(whereis_mock, pid),
	?assertEqual(pid, ?whereis(supervisor)).

init_test() ->
	erlang:put(children_mock, [1, 2, 3]),
	erlang:put(whereis_mock, pid),
	TestState = #state{
		supervisor 					= supervisor,
		pids_count_original = 3,
		min_alive_ratio 		= 0.75,
		pid_table 					= supervisor_pid_table,
		last_pid 						= 3}.

balance_test() ->
	erlang:put(children_mock, [1, 2, 3]),
	erlang:put(whereis_mock, pid),
	{ok, StateInit} = init({supervisor, 0.75}),
	{reply, Pid1, State1} = handle_call({pid}, x, StateInit),
	?assertEqual(1, Pid1),
	{reply, Pid2, State2} = handle_call({pid}, x, State1),
	?assertEqual(2, Pid2),
	{reply, Pid3, State3} = handle_call({pid}, x, State2),
	?assertEqual(3, Pid3),
	{reply, Pid4, _State4} = handle_call({pid}, x, State3),
	?assertEqual(1, Pid4).

apply_within_test() ->
	erlang:put(children_mock, [1, 2, 3]),
	erlang:put(whereis_mock, pid),
	{ok, StateInit} = init({supervisor, 0.75}),
	{reply, Reply, StateInit} = handle_call({apply_within, {lists, reverse, [[1, 2]]}, 0}, x, StateInit),
	?assertEqual([2, 1], Reply),
	{reply, Reply2, StateInit} = handle_call({apply_within, {lists, reverse, [[2, 1]]}, 100}, x, StateInit),
	?assertEqual([1, 2], Reply2),
	{reply, Reply3, StateInit} = handle_call({apply_within, {lists, reverse, [[2, 3]]}, fun(P) -> P end}, x, StateInit),
	?assertEqual([3, 2], Reply3).

no_supervisor_init_test() ->
	erlang:put(whereis_mock, undefined),
	Error =
	try
		init({supervisor, 0.75})
	catch
		exit:Reason -> Reason
	end,
	?assertEqual({error, supervisor_not_running}, Error).

no_children_init_test() ->
	erlang:put(whereis_mock, pid),
	erlang:put(children_mock, []),
	Error =
	try
		init({supervisor, 0.75})
	catch
		exit:Reason -> Reason
	end,
	?assertEqual({error, supervisor_has_no_children}, Error).

no_children_test() ->
	erlang:put(whereis_mock, pid),
	erlang:put(children_mock, [1, 2]),
	{ok, StateInit} = init({supervisor, 0.75}),
	erlang:put(children_mock, []),
	Error =
	try
		{noreply, StateDown} =
		handle_info({'DOWN', x, x, 1, x}, StateInit),
		handle_info({'DOWN', x, x, 2, x}, StateDown)
	catch
		exit:Reason -> Reason
	end,
	?assertEqual({error, supervisor_has_no_children}, Error).

no_supervisor_test() ->
	erlang:put(whereis_mock, pid),
	erlang:put(children_mock, [1, 2]),
	{ok, StateInit} = init({supervisor, 0.75}),
	erlang:put(whereis_mock, undefined),
	Error =
	try
		handle_info({'DOWN', x, x, 1, x}, StateInit)
	catch
		exit:Reason -> Reason
	end,
	?assertEqual({error, supervisor_not_running}, Error).

exit_test() ->
	erlang:put(children_mock, [1, 2, 3]),
	erlang:put(whereis_mock, pid),
	{ok, StateInit} = init({supervisor, 0.75}),
	erlang:put(children_mock, [1, 3]),
	{noreply, StateDown} = handle_info({'DOWN', x, x, 2, x}, StateInit),
	{reply, Pid1, State1} = handle_call({pid}, x, StateDown),
	?assertEqual(1, Pid1),
	{reply, Pid2, State2} = handle_call({pid}, x, State1),
	?assertEqual(3, Pid2),
	{reply, Pid3, State3} = handle_call({pid}, x, State2),
	?assertEqual(1, Pid3),
	erlang:put(children_mock, [1]),
	{noreply, StateDown2} = handle_info({'DOWN', x, x, 3, x}, State3),
	{reply, Pid4, State4} = handle_call({pid}, x, StateDown2),
	?assertEqual(1, Pid4),
	{reply, Pid5, _State5} = handle_call({pid}, x, State4),
	?assertEqual(1, Pid5).

-endif.
