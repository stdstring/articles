-module(parallel_pool_reduce_tests).

-include_lib("eunit/include/eunit.hrl").

pool_reduce_test_() ->
	PoolNodes = ['node1@pooltester', 'node2@pooltester'],
	[{"empty source", ?_assertEqual(1, parallel_reduce:pool_reduce(fun(Item, Agg) -> Item + Agg end, [], {1, 0}, 2, 2, PoolNodes))},
	 pool_reduce_integer_list(PoolNodes),
	 pool_reduce_string_list(PoolNodes)].

pool_reduce_integer_list(PoolNodes) ->
	[{"integer list with small list size", ?_assertEqual(4, parallel_reduce:pool_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2], {1, 0}, 4, 2, PoolNodes))},
	 {"integer list with with size equals portion size", ?_assertEqual(7, parallel_reduce:pool_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 3, 2, PoolNodes))},
	 {"integer list with with multiple portion", ?_assertEqual(11, parallel_reduce:pool_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3, 4], {1, 0}, 2, 2, PoolNodes))},
	 {"integer list with with nonmultiple portion", ?_assertEqual(7, parallel_reduce:pool_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 2, 2, PoolNodes))},
	 {"integer list with with one worker", ?_assertEqual(7, parallel_reduce:pool_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 2, 1, PoolNodes))}].

pool_reduce_string_list(PoolNodes) ->
	[{"string list with small list size", ?_assertEqual("++aabb", parallel_reduce:pool_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb"], {"++", ""}, 4, 2, PoolNodes))},
	 {"string list with with size equals portion size", ?_assertEqual("++aabbcc", parallel_reduce:pool_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 3, 2, PoolNodes))},
	 {"string list with with multiple portion", ?_assertEqual("++aabbccdd", parallel_reduce:pool_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc", "dd"], {"++", ""}, 2, 2, PoolNodes))},
	 {"string list with with nonmultiple portion", ?_assertEqual("++aabbcc", parallel_reduce:pool_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 2, 2, PoolNodes))},
	 {"string list with with one worker", ?_assertEqual("++aabbcc", parallel_reduce:pool_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 2, 1, PoolNodes))}].