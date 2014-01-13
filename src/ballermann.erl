-module (ballermann).

-ifdef(TEST).
-compile([export_all]).
-endif.

-behaviour (gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([balance/2, balance/3, apply_within/2, apply_within/3, start_link/3, pid/1, child_pids/1]).

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
    PidTable = ets:new(pid_table, [private, duplicate_bag]),
    State = #state{
        supervisor          = Supervisor,
        pids_count_original = undefined,
        min_alive_ratio     = MinAliveRatio,
        pid_table           = PidTable,
        last_pid            = undefined},
    gen_server:cast(self(), add_missing_pids),
    {ok, State}.

handle_call({pid}, _From, State = #state{last_pid = LastPid, pid_table = PidTable}) ->
    Pid = case ets:next(PidTable, LastPid) of
        '$end_of_table' ->
            ets:first(PidTable);
        Value ->
            Value
    end,
    {reply, Pid, State#state{last_pid = Pid}};

handle_call({apply_within, {Module, Function, Args}, WaitTimeOrFun}, _From, State = #state{supervisor = Supervisor}) ->
    case WaitTimeOrFun of
        WaitTime when is_float(WaitTime) orelse is_integer(WaitTime) ->
            timer:sleep(WaitTime);
        Fun when is_function(Fun) ->
            [Fun(Pid) || Pid <- ?MODULE:child_pids(Supervisor)]
    end,
    Reply = apply(Module, Function, Args),
    {reply, Reply, State}.

handle_cast(add_missing_pids, State = #state{ supervisor = Supervisor, pid_table = PidTable }) ->
    add_missing_pids(PidTable, Supervisor),
    FirstPid  = ets:first(PidTable),
    TableSize = table_size(PidTable),
    {noreply, State#state{ last_pid = FirstPid, pids_count_original = TableSize }}.

handle_info({'DOWN', _, _, Pid, _}, State = #state{supervisor = Supervisor, last_pid = LastPid, pid_table = PidTable, pids_count_original = PidsCountOriginal, min_alive_ratio = MinAliveRatio}) ->
    error_logger:info_msg("~p: The process ~p (child of ~p) died.\n", [?MODULE, Pid, Supervisor]),
    ets:delete(PidTable, Pid),
    case too_few_pids(PidTable, PidsCountOriginal, MinAliveRatio) of
        true ->
            error_logger:warning_msg("~p: Reloading children from supervisor ~p.\n", [?MODULE, Supervisor]),
            add_missing_pids(PidTable, Supervisor);
        false ->
            noop
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
        _ -> noop
    end.

too_few_pids(PidTable, PidsCountOriginal, MinAliveRatio) ->
    table_size(PidTable) / PidsCountOriginal < MinAliveRatio.

add_missing_pids(Table, Supervisor) ->
    Pids = ?MODULE:child_pids(Supervisor),
    PidsNew = lists:filter(fun(E) -> ets:lookup(Table, E) =:= [] end, Pids),
    error_logger:info_msg("~p: Found ~p new processes of ~p total.\n", [?MODULE, length(PidsNew), length(Pids)]),
    PidsWithRefs = [{Pid, {monitor(Pid)}}|| Pid <- PidsNew],
    ets:insert(Table, PidsWithRefs),
    check_zero_pids(Table, Supervisor).

-ifdef(TEST).
monitor(_) -> ok.
-else.
monitor(Pid) -> erlang:monitor(process, Pid).
-endif.

child_pids(Supervisor) ->
    case alive(Supervisor) of
        false ->
            error_logger:error_msg("~p Supervisor ~p not running. Giving up.\n", [?MODULE, Supervisor]),
            exit({error, supervisor_not_running});
        _ ->
            [ Pid || {_, Pid, _, _} <- supervisor:which_children(Supervisor), is_pid(Pid)]
    end.

alive(undefined) ->
    false;
alive(Supervisor) when is_atom(Supervisor) ->
    alive(erlang:whereis(Supervisor));
alive(Supervisor) when is_pid(Supervisor) ->
    erlang:is_process_alive(Supervisor).

table_size(Table) ->
        {size, Count} = proplists:lookup(size, ets:info(Table)),
        Count.
