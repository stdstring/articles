-module(parallel_filter_tests).

-include_lib("eunit/include/eunit.hrl").

portion_gen_filter_test_() ->
	[{"empty source", ?_assertEqual([], parallel_filter:portion_gen_filter(fun(_Item) -> true end, [], 2))},
	 portion_gen_filter_integer_list(),
	 portion_gen_filter_string_list()].

portion_gen_filter_integer_list() ->
	[{"integer list with small list size", ?_assertEqual([8], parallel_filter:portion_gen_filter(fun(Item) -> Item > 5 end, [1, 8, 5], 4))},
	 {"integer list with size equals portion size", ?_assertEqual([8], parallel_filter:portion_gen_filter(fun(Item) -> Item > 5 end, [1, 8, 5], 3))},
	 {"integer list with multiple portion", ?_assertEqual([8, 7], parallel_filter:portion_gen_filter(fun(Item) -> Item > 5 end, [1, 8, 5, 7, 3, 4], 2))},
	 {"integer list with nonmultiple portion", ?_assertEqual([8, 7], parallel_filter:portion_gen_filter(fun(Item) -> Item > 5 end, [1, 8, 5, 7, 3], 2))}].

portion_gen_filter_string_list() ->
	[{"string list with small list size", ?_assertEqual(["bbb"], parallel_filter:portion_gen_filter(fun(Item) -> length(Item) > 2 end, ["a", "bbb", "cc"], 4))},
	 {"string list with size equals portion size", ?_assertEqual(["bbb"], parallel_filter:portion_gen_filter(fun(Item) -> length(Item) > 2 end, ["a", "bbb", "cc"], 3))},
	 {"string list with multiple portion", ?_assertEqual(["bbb", "dddd"], parallel_filter:portion_gen_filter(fun(Item) -> length(Item) > 2 end, ["a", "bbb", "cc", "dddd", "e", "f"], 2))},
	 {"string list with nonmultiple portion", ?_assertEqual(["bbb", "dddd"], parallel_filter:portion_gen_filter(fun(Item) -> length(Item) > 2 end, ["a", "bbb", "cc", "dddd", "e"], 2))}].