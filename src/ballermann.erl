-module (ballermann).
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
	PidTable = ets:new(pid_store, [private, duplicate_bag]),
	add_missing_pids(PidTable, Supervisor),
	State = #state{
		supervisor 					= Supervisor,
		pids_count_original = table_size(PidTable),
		min_alive_ratio 		= MinAliveRatio,
		pid_table 					= PidTable,
		last_pid 						= ets:first(PidTable)},
	{ok, State}.

handle_call({pid}, _From, State = #state{last_pid = LastPid, pid_table = PidTable}) ->
	Pid = case ets:next(PidTable, LastPid) of
		Value when is_pid(Value) ->
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
			add_missing_pids(PidTable, Supervisor),
			case table_size(PidTable) of
				0 -> 
					error_logger:errror_msg("~p: Supervisor ~p has no children. Giving up.\n", [?MODULE, Supervisor]),
					exit({supervisor_has_no_children});
				_ -> ignore
			end;
		false -> ignore
	end,
	% Pick a valid LastPid, the recent one might be the one which just died.
	LastPidSave = case LastPid of
		Pid ->
			ets:first(PidTable);
		_ ->
			LastPid
	end,
	{noreply, State#state{last_pid = LastPidSave}};
	
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

too_few_pids(PidTable, PidsCountOriginal, MinAliveRatio) ->
	table_size(PidTable) / PidsCountOriginal < MinAliveRatio.

add_missing_pids(Table, Supervisor) ->
	Pids = child_pids(Supervisor),
	PidsNew = lists:filter(fun(E) -> ets:lookup(Table, E) =:= [] end, Pids),
	error_logger:info_msg("~p: Found ~p new processes of ~p total.\n", [?MODULE, length(PidsNew), length(Pids)]),
	PidsWithRefs = [{Pid, {monitor(process, Pid)}}|| Pid <- PidsNew],
	ets:insert(Table, PidsWithRefs).

child_pids(Supervisor) ->
	case whereis(Supervisor) of
		undefined ->
			error_logger:errror_msg("~p Supervisor ~p disappeared. Giving up.\n", [?MODULE, Supervisor]),
			exit({supervisor_disappeared});
		_ ->
				[ Pid || {_, Pid, _, _} <- supervisor:which_children(Supervisor)]
	end.

table_size(Table) ->
		{size, Count} = proplists:lookup(size, ets:info(Table)),
		Count.