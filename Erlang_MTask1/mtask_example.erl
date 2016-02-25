-module(mtask_example).

-export([example/0, example2/0]).

example() ->
	Master = self(),
	Worker = spawn(fun() -> example_worker(Master) end),
	Worker ! hello,
	Worker ! {ping, Master},
	Worker ! hi,
	receive
		timeout -> io:format("timeout in worker ~n")
	end,
	receive
		{pong, Worker} -> io:format("pong from worker ~n")
	end.

example_worker(Master) ->
	receive
		{ping, From} ->
			io:format("ping from master ~n"),
			From ! {pong, self()},
			example_worker(Master);
		_Other ->
			io:format("unknown message ~n"),
			example_worker(Master)
	after 1000 ->
		io:format("timeout ~n"),
		erlang:send(Master, timeout)
	end.

example2() ->
	Master = self(),
	Worker = spawn(fun() -> example2_worker(Master) end),
	Worker ! message2,
	receive
		ready -> nothing
	end,
	Worker ! message1,
	receive
		work_complete -> nothing
	end,
	io:format("work complete on master side ~n").

example2_worker(Master) ->
	receive
		message1 -> nothing
	end,
	Master ! ready,
	receive
		message2 -> nothing
	end,
	Master ! work_complete,
	io:format("work complete on worker side ~n").