-module(interlock_ex).

-export([fun1/0, fun2/0]).

fun1() ->
	receive
		{init, Process2} -> io:format("init message ~n", [])
	end,
	io:format("process 1, send message a ~n", []),
	Process2 ! {self(), a},
	receive
		{Process2, a} -> io:format("a message on process 1 ~n", [])
	end,
	io:format("process 1, send message b ~n", []),
	Process2 ! {self(), b},
	receive
		{Process2, b} -> io:format("b message on process 1 ~n", [])
	end.

fun2() ->
	receive
		{Process1, b} -> io:format("b message on process 2 ~n", [])
	end,
	io:format("process 2, send message b ~n", []),
	Process1 ! {self(), b},	
	receive
		{Process1, a} -> io:format("a message on process 2 ~n", [])
	end,
	io:format("process 2, send message a ~n", []),
	Process1 ! {self(), a}.