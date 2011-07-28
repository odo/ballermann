-module (ballermann).
-behaviour (gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([start_link/3, pid/1]).
-type sup_ref()  :: {atom(), atom()}.
-type pids() :: [pid()].

-record(state, {
	supervisor :: sup_ref(),
	pid_table :: pids(),
	last_pid :: pids(),
	pids_count_original :: integer(),
	min_alive_ratio :: float()
	}).

start_link(Supervisor, ServerName, MinAliveRatio) ->
	gen_server:start_link({local, ServerName}, ?MODULE, {Supervisor, MinAliveRatio}, []).

pid(ServerName) ->
	gen_server:call(ServerName, {pid}).

init({Supervisor, MinAliveRatio}) ->
	PidTable = ets:new(dunlicate_bag, [private]),
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
	io:format("The process died: ~p\n", [Pid]),
	ets:delete(PidTable, Pid),
	case too_few_pids(PidTable, PidsCountOriginal, MinAliveRatio) of
		true -> add_missing_pids(PidTable, Supervisor);
		false -> nothing_to_do
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
	io:format("~p Adding ~p Pids.\n", [?MODULE, length(PidsNew)]),
	PidsWithRefs = [{Pid, {monitor(process, Pid)}}|| Pid <- PidsNew],
	ets:insert(Table, PidsWithRefs).

child_pids(Supervisor) ->
	[ Pid || {_, Pid, _, _} <- supervisor:which_children(Supervisor)].

table_size(Table) ->
		{size, Count} = proplists:lookup(size, ets:info(Table)),
		Count.