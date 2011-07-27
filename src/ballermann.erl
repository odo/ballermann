-module (ballermann).
-behaviour (gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([start_link/4, pid/1]).
-type sup_ref()  :: {atom(), atom()}.
-type pids() :: [pid()].

-record(state, {
	supervisor :: sup_ref(),
	pids :: pids(),
	old_pids :: pids(),
	index :: integer(),
	calls_since_update :: integer(),
	refresh_every_calls :: integer()
	}).

start_link(Supervisor, ServerName, RefreshEveryCall, RefreshEveryMs) ->
	Res = gen_server:start_link({local, ServerName}, ?MODULE, {Supervisor, RefreshEveryCall}, []),
	timer:send_interval(RefreshEveryMs, whereis(ServerName), update_pids),
	Res.

pid(ServerName) ->
	gen_server:call(ServerName, pid).

init({Supervisor, RefreshEveryCall}) ->
	State = update_pids(#state{supervisor = Supervisor, index = 0, refresh_every_calls = RefreshEveryCall}),
	{ok, State}.

handle_call(pid, _From, StateIn) ->
	State = update_pids(StateIn),
	case State of
		#state{pids = [], old_pids = []} ->
			throw({error, supervisor_has_no_children});
		#state{pids = [], old_pids = OldPids} ->
			[Reply | Pids] = lists:reverse(OldPids),
			{reply, Reply, State#state{old_pids=[Reply], pids = Pids}};
		#state{pids = [Reply | Pids], old_pids = OldPids} ->
			{reply, Reply, State#state{old_pids=[Reply | OldPids], pids = Pids}}
	end.

update_pids(State = #state{refresh_every_calls = Refresh, calls_since_update = Calls}) when Calls > Refresh ->
	State#state{pids = child_pids(State#state.supervisor), calls_since_update = 0};
update_pids(State) ->
	State.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(timeout, State) -> {update_pids, update_pids(State)};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

child_pids(Supervisor) ->
	[ Pid || {_, Pid, _, _} <- supervisor:which_children(Supervisor)].

