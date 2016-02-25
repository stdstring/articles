-module(load_sample).

-export([create_load/1, ping/0, pong/0]).

create_load(0) -> true;
create_load(Count) ->
	PingPID = spawn(load_sample, ping, []),
	PongPID = spawn(load_sample, pong, []),
	PingPID ! {ping, PongPID},
	create_load(Count-1).

ping() ->
	receive
		{ping, PID} -> PID ! {pong, self()},
					   ping()
	end.

pong() ->
	receive
		{pong, PID} -> PID ! {ping, self()},
					   pong()
	end.