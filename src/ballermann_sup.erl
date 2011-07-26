-module(ballermann_sup).
-behaviour(supervisor).

%% API
-export ([start_link/2, start_link/4]).

%% Callbacks
-export ([init/1]).

-define(REFRESH_EVERY_CALLS, 10000).
-define(REFRESH_EVERY_MS,    5000).

start_link(Supervisor, ServerName) ->
	supervisor:start_link({local, supervisor_name(ServerName)}, ?MODULE, {Supervisor, ServerName, ?REFRESH_EVERY_CALLS, ?REFRESH_EVERY_MS}).

start_link(Supervisor, ServerName, RefreshEveryCall, RefreshEveryMs) ->
	supervisor:start_link({local, supervisor_name(ServerName)}, ?MODULE, {Supervisor, ServerName, RefreshEveryCall, RefreshEveryMs}).

init({Supervisor, ServerName, RefreshEveryCall, RefreshEveryMs}) ->
	Server = {ServerName, {ballermann, start_link, [Supervisor, ServerName, RefreshEveryCall, RefreshEveryMs]},
						permanent, 1000, worker, [ballermann]},
	Children = [Server],
	RestartStrategy = {one_for_one, 10, 1},
	{ok, {RestartStrategy, Children}}.
	
supervisor_name(ServerName) ->
	list_to_atom(string:concat(atom_to_list(ServerName), "_sub")).