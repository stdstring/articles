-module(parallel_pool_map_tests).

-include_lib("eunit/include/eunit.hrl").

pool_pmap_test_() ->
	PoolNodes = ['node1@pooltester', 'node2@pooltester'],
	[{"empty source", ?_assertEqual([], parallel_map:pool_pmap(fun(Item) -> Item end, [], 2, 2, PoolNodes))},
	 pool_pmap_integer_list(PoolNodes),
	 pool_pmap_string_list(PoolNodes)].

pool_pmap_integer_list(PoolNodes) ->
	[{"integer list with small list size", ?_assertEqual([3, 6, 15], parallel_map:pool_pmap(fun(Item) -> 3*Item end, [1, 2, 5], 4, 4, PoolNodes))},
	 {"integer list with size equals portion size", ?_assertEqual([3, 6, 15], parallel_map:pool_pmap(fun(Item) -> 3*Item end, [1, 2, 5], 3, 3, PoolNodes))},
	 {"integer list with multiple portion", ?_assertEqual([3, 6, 15, 24, 21, 27], parallel_map:pool_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9], 2, 2, PoolNodes))},
	 {"integer list with nonmultiple portion", ?_assertEqual([3, 6, 15, 24, 21, 27, 12], parallel_map:pool_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9, 4], 2, 2, PoolNodes))},
	 {"integer list with one worker", ?_assertEqual([3, 6, 15, 24, 21, 27, 12], parallel_map:pool_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9, 4], 2, 1, PoolNodes))}].

pool_pmap_string_list(PoolNodes) ->
	[{"string list with small list size", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:pool_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 4, 4, PoolNodes))},
	 {"string list with size equals portion size", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:pool_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 3, 3, PoolNodes))},
	 {"string list with multiple portion", ?_assertEqual(["31rts", "666rts", "99rts", "7rts"], parallel_map:pool_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99", "str7"], 2, 2, PoolNodes))},
	 {"string list with nonmultiple portion", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:pool_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 2, 2, PoolNodes))},
	 {"string list with one worker", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:pool_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 2, 1, PoolNodes))}].