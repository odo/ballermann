-module(ballermann_sup).
-behaviour(supervisor).

%% API
-export ([start_link/2, start_link/3]).

%% Callbacks
-export ([init/1]).

-define(MIN_ALIVE_RATIO, 0.8).

start_link(Supervisor, ServerName) ->
	start_link(Supervisor, ServerName, ?MIN_ALIVE_RATIO).

start_link(Supervisor, ServerName, MinAliveRatio) ->
	supervisor:start_link({local, supervisor_name(ServerName)}, ?MODULE, {Supervisor, ServerName, MinAliveRatio}).

init({Supervisor, ServerName, MinAliveRatio}) ->
	Server = {ServerName, {ballermann, start_link, [Supervisor, ServerName, MinAliveRatio]},
						permanent, 1000, worker, [ballermann]},
	Children = [Server],
	RestartStrategy = {one_for_one, 10, 1},
	{ok, {RestartStrategy, Children}}.
	
supervisor_name(ServerName) ->
	list_to_atom(string:concat(atom_to_list(ServerName), "_sub")).