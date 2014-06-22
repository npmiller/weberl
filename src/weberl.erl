-module(weberl).
-behavior(application).
-export([start/2, stop/1]).

-behavior(supervisor).
-export([start_link/0, start_listener/0]).
-export([init/1]).

-include("weberl.hrl").

start(normal, _Args) ->
	start_link().

stop(_State) ->
	ok.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, Port} = application:get_env(port),
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, false}]),
	spawn_link(fun initial_listeners/0),
	{ok, {{simple_one_for_one, 60, 3600},
		[{socket,
		{weberl_serv, start_link, [Socket]}, %Pass the socket!
		temporary, 1000, worker, [weberl_serv]}
		]}}.

start_listener() ->
	supervisor:start_child(?MODULE,[]).

initial_listeners() ->
	[ start_listener() || _ <- lists:seq(1,20)],
	ok.
