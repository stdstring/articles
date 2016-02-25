-module(rpc_example).

-export([do/0]).

do() ->
	io:format("do executing on ~p~n", [node()]),
	{result, 666}.