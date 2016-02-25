-module(parallel_map_tests).

-include_lib("eunit/include/eunit.hrl").

usual_map_test_() ->
	[{"empty source", ?_assertEqual([], parallel_map:usual_map(fun(Item) -> Item end, []))},
	 {"integer list", ?_assertEqual([3, 6, 15, 24], parallel_map:usual_map(fun(Item) -> 3*Item end, [1, 2, 5, 8]))},
	 {"string list", ?_assertEqual(["31rts", "666rts"], parallel_map:usual_map(fun(Item) -> lists:reverse(Item) end, ["str13", "str666"]))}].

simple_pmap_test_() ->
	[{"empty source", ?_assertEqual([], parallel_map:simple_pmap(fun(Item) -> Item end, []))},
	 {"integer list", ?_assertEqual([3, 6, 15, 24], parallel_map:simple_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8]))},
	 {"string list", ?_assertEqual(["31rts", "666rts"], parallel_map:simple_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666"]))}].

portion_pmap_test_() ->
	[{"empty source", ?_assertEqual([], parallel_map:portion_pmap(fun(Item) -> Item end, [], 2))},
	 portion_pmap_integer_list(),
	 portion_pmap_string_list()].

portion_pmap_integer_list() ->
	[{"integer list with small list size", ?_assertEqual([3, 6, 15], parallel_map:portion_pmap(fun(Item) -> 3*Item end, [1, 2, 5], 4))},
	 {"integer list with size equals portion size", ?_assertEqual([3, 6, 15], parallel_map:portion_pmap(fun(Item) -> 3*Item end, [1, 2, 5], 3))},
	 {"integer list with multiple portion", ?_assertEqual([3, 6, 15, 24, 21, 27], parallel_map:portion_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9], 2))},
	 {"integer list with nonmultiple portion", ?_assertEqual([3, 6, 15, 24, 21, 27, 12], parallel_map:portion_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9, 4], 2))}].

portion_pmap_string_list() ->
	[{"string list with small list size", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:portion_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 4))},
	 {"string list with size equals portion size", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:portion_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 3))},
	 {"string list with multiple portion", ?_assertEqual(["31rts", "666rts", "99rts", "7rts"], parallel_map:portion_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99", "str7"], 2))},
	 {"string list with nonmultiple portion", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:portion_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 2))}].

portion_gen_pmap_test_() ->
	[{"empty source", ?_assertEqual([], parallel_map:portion_gen_pmap(fun(Item) -> Item end, [], 2))},
	 portion_gen_pmap_integer_list(),
	 portion_gen_pmap_string_list()].

portion_gen_pmap_integer_list() ->
	[{"integer list with small list size", ?_assertEqual([3, 6, 15], parallel_map:portion_gen_pmap(fun(Item) -> 3*Item end, [1, 2, 5], 4))},
	 {"integer list with size equals portion size", ?_assertEqual([3, 6, 15], parallel_map:portion_gen_pmap(fun(Item) -> 3*Item end, [1, 2, 5], 3))},
	 {"integer list with multiple portion", ?_assertEqual([3, 6, 15, 24, 21, 27], parallel_map:portion_gen_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9], 2))},
	 {"integer list with nonmultiple portion", ?_assertEqual([3, 6, 15, 24, 21, 27, 12], parallel_map:portion_gen_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9, 4], 2))}].

portion_gen_pmap_string_list() ->
	[{"string list with small list size", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:portion_gen_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 4))},
	 {"string list with size equals portion size", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:portion_gen_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 3))},
	 {"string list with multiple portion", ?_assertEqual(["31rts", "666rts", "99rts", "7rts"], parallel_map:portion_gen_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99", "str7"], 2))},
	 {"string list with nonmultiple portion", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:portion_gen_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 2))}].

limited_pmap_test_() ->
	[{"empty source", ?_assertEqual([], parallel_map:limited_pmap(fun(Item) -> Item end, [], 2, 2))},
	 limited_pmap_integer_list(),
	 limited_pmap_string_list()].

limited_pmap_integer_list() ->
	[{"integer list with small list size", ?_assertEqual([3, 6, 15], parallel_map:limited_pmap(fun(Item) -> 3*Item end, [1, 2, 5], 4, 4))},
	 {"integer list with size equals portion size", ?_assertEqual([3, 6, 15], parallel_map:limited_pmap(fun(Item) -> 3*Item end, [1, 2, 5], 3, 3))},
	 {"integer list with multiple portion", ?_assertEqual([3, 6, 15, 24, 21, 27], parallel_map:limited_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9], 2, 2))},
	 {"integer list with nonmultiple portion", ?_assertEqual([3, 6, 15, 24, 21, 27, 12], parallel_map:limited_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9, 4], 2, 2))},
	 {"integer list with one worker", ?_assertEqual([3, 6, 15, 24, 21, 27, 12], parallel_map:limited_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9, 4], 2, 1))}].

limited_pmap_string_list() ->
	[{"string list with small list size", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:limited_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 4, 4))},
	 {"string list with size equals portion size", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:limited_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 3, 3))},
	 {"string list with multiple portion", ?_assertEqual(["31rts", "666rts", "99rts", "7rts"], parallel_map:limited_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99", "str7"], 2, 2))},
	 {"string list with nonmultiple portion", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:limited_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 2, 2))},
	 {"string list with one worker", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:limited_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 2, 1))}].

distributed_pmap_test_() ->
	[{"empty source", ?_assertEqual([], parallel_map:distributed_pmap(fun(Item) -> Item end, [], 2, [node(), node()], 2))},
	 distributed_pmap_integer_list(),
	 distributed_pmap_string_list()].

distributed_pmap_integer_list() ->
	[{"integer list with small list size", ?_assertEqual([3, 6, 15], parallel_map:distributed_pmap(fun(Item) -> 3*Item end, [1, 2, 5], 4, [node()], 2))},
	 {"integer list with size equals portion size", ?_assertEqual([3, 6, 15], parallel_map:distributed_pmap(fun(Item) -> 3*Item end, [1, 2, 5], 3, [node()], 2))},
	 {"integer list with multiple portion", ?_assertEqual([3, 6, 15, 24, 21, 27], parallel_map:distributed_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9], 2, [node()], 2))},
	 {"integer list with nonmultiple portion", ?_assertEqual([3, 6, 15, 24, 21, 27, 12], parallel_map:distributed_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9, 4], 2, [node()], 2))},
	 {"integer list with one worker", ?_assertEqual([3, 6, 15, 24, 21, 27, 12], parallel_map:distributed_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9, 4], 2, [node()], 1))},
	 {"integer list with several nodes", ?_assertEqual([3, 6, 15, 24, 21, 27], parallel_map:distributed_pmap(fun(Item) -> 3*Item end, [1, 2, 5, 8, 7, 9], 2, [node(), node()], 2))}].

distributed_pmap_string_list() ->
	[{"string list with small list size", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:distributed_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 4, [node()], 2))},
	 {"string list with size equals portion size", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:distributed_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 3, [node()], 2))},
	 {"string list with multiple portion", ?_assertEqual(["31rts", "666rts", "99rts", "7rts"], parallel_map:distributed_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99", "str7"], 2, [node()], 2))},
	 {"string list with nonmultiple portion", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:distributed_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 2, [node()], 2))},
	 {"string list with one worker", ?_assertEqual(["31rts", "666rts", "99rts"], parallel_map:distributed_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99"], 2, [node()], 1))},
	 {"string list with several nodes", ?_assertEqual(["31rts", "666rts", "99rts", "7rts"], parallel_map:distributed_pmap(fun(Item) -> lists:reverse(Item) end, ["str13", "str666", "str99", "str7"], 2, [node(), node()], 2))}].