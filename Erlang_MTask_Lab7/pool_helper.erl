-module(pool_helper).

-export([start_pool/2, stop_pool/0]).

start_pool(Prefix, PoolNodes) ->
	pool:start(Prefix),
	lists:foreach(fun(Node) -> pool:attach(Node) end, PoolNodes).

stop_pool() ->
	pool:stop().