-module (ballermann).
-behaviour (gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([start_link/4, pid/1]).
-type sup_ref()  :: {atom(), atom()}.
-type pids() :: [pid()].

-record(state, {
	supervisor :: sup_ref(),
	pids :: pids(),
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
	Refresh = StateIn#state.refresh_every_calls,
	State = 
	case StateIn#state.calls_since_update of
		Calls when Calls > Refresh ->
			 update_pids(StateIn);
		Calls when Calls =< Refresh ->
			StateIn
	end,
	Index = State#state.index + 1,
	Pids = State#state.pids,
	IndexSave = 
	case length(Pids) of
		0 ->
			throw({error, supervisor_has_no_children});
		N ->
			case Index > N of
				true ->
					1;
				false ->
					Index
			end
	end,
	Reply = lists:nth(IndexSave, Pids),
	{reply, Reply, State#state{index = IndexSave, calls_since_update = (State#state.calls_since_update + 1)}}.

update_pids(State) ->
	State#state{pids = child_pids(State#state.supervisor), calls_since_update = 0}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(timeout, State) -> {update_pids, update_pids(State)};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

child_pids(Supervisor) ->
	[ Pid || {_, Pid, _, _} <- supervisor:which_children(Supervisor)].

