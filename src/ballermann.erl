-module (ballermann).

-ifdef(BALLERMANNTEST).
	-include_lib("eunit/include/eunit.hrl").
	-define(supervisor_which_children(Supervisor), which_children_mock()).
	-define(whereis(Supervisor), whereis_mock()).
	-define(monitor(Type, Pid), monitor).
	-define(is_pid(Pid), is_integer(Pid)).
-else.
	-define(supervisor_which_children(Supervisor), supervisor:which_children(Supervisor)).
	-define(whereis(Supervisor), whereis(Supervisor)).
	-define(monitor(Type, Pid), monitor(Type, Pid)).
	-define(is_pid(Pid), is_pid(Pid)).
-endif.

-behaviour (gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([balance/2, balance/3, start_link/3, pid/1]).
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

init({Supervisor, MinAliveRatio}) ->
	PidTableName = list_to_atom(atom_to_list(Supervisor) ++ "_pid_table"),
	ets:new(PidTableName, [private, duplicate_bag, named_table]),
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
	{reply, Pid, State#state{last_pid = Pid}}.

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
		last_pid 						= 3},
	?assertEqual({ok, TestState}, init({supervisor, 0.75})),
	?assertEqual([{1,{monitor}},{2,{monitor}},{3,{monitor}}], ets:tab2list(supervisor_pid_table)),
	ets:delete(supervisor_pid_table).

balance_test() ->
	erlang:put(children_mock, [1, 2, 3]),
	erlang:put(whereis_mock, pid),
	{ok, StateInit} = init({supervisor, 0.75}),
	{reply, Pid1, State1} = handle_call({pid}, x, StateInit),
	?assertEqual(2, Pid1),
	{reply, Pid2, State2} = handle_call({pid}, x, State1),
	?assertEqual(1, Pid2),
	{reply, Pid3, State3} = handle_call({pid}, x, State2),
	?assertEqual(3, Pid3),
	{reply, Pid4, _State4} = handle_call({pid}, x, State3),
	?assertEqual(2, Pid4),
	ets:delete(supervisor_pid_table).

no_supervisor_init_test() ->
	erlang:put(whereis_mock, undefined),
	Error = 
	try
		init({supervisor, 0.75})
	catch
		exit:Reason -> Reason
	end,
	?assertEqual({error, supervisor_not_running}, Error),
	ets:delete(supervisor_pid_table).

no_children_init_test() ->
	erlang:put(whereis_mock, pid),
	erlang:put(children_mock, []),
	Error = 
	try
		init({supervisor, 0.75})
	catch
		exit:Reason -> Reason
	end,
	?assertEqual({error, supervisor_has_no_children}, Error),
	ets:delete(supervisor_pid_table).

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
	?assertEqual({error, supervisor_has_no_children}, Error),
	ets:delete(supervisor_pid_table).

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
	?assertEqual({error, supervisor_not_running}, Error),
	ets:delete(supervisor_pid_table).

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
	?assertEqual(1, Pid5),
	ets:delete(supervisor_pid_table).

reload_test() ->
	erlang:put(children_mock, [1, 2, 3, 4, 5]),
	erlang:put(whereis_mock, pid),
	%  we are defining a 60% threshold, so it should reload after three processes have died 
	{ok, StateInit} = init({supervisor, 0.60}),
	erlang:put(children_mock, [11, 2, 13, 4, 15]),
	{noreply, StateDown1} = handle_info({'DOWN', x, x, 3, x}, StateInit),
	{noreply, StateDown2} = handle_info({'DOWN', x, x, 5, x}, StateDown1),
	% although we changed the children ballermann still sees the old set
	{reply, Pid1, State1} = handle_call({pid}, x, StateDown2),
	?assertEqual(1, Pid1),
	{reply, Pid2, State2} = handle_call({pid}, x, State1),
	?assertEqual(4, Pid2),
	{reply, Pid3, State3} = handle_call({pid}, x, State2),
	?assertEqual(2, Pid3),
	{reply, Pid4, State4} = handle_call({pid}, x, State3),
	?assertEqual(1, Pid4),
	% now after the third process dies, a reload is performed
	{noreply, StateDown3} = handle_info({'DOWN', x, x, 1, x}, State4),
	{reply, Pid5, State5} = handle_call({pid}, x, StateDown3),
	?assertEqual(13, Pid5),	
	{reply, Pid6, State6} = handle_call({pid}, x, State5),
	?assertEqual(11, Pid6),	
	{reply, Pid7, State7} = handle_call({pid}, x, State6),
	?assertEqual(4, Pid7),	
	{reply, Pid8, State8} = handle_call({pid}, x, State7),
	?assertEqual(15, Pid8),	
	{reply, Pid9, State9} = handle_call({pid}, x, State8),
	?assertEqual(2, Pid9),	
	{reply, Pid10, _State10} = handle_call({pid}, x, State9),
	?assertEqual(13, Pid10),	
	ets:delete(supervisor_pid_table).	 	

-endif.