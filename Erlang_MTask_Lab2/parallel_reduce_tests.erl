-module(parallel_reduce_tests).

-include_lib("eunit/include/eunit.hrl").

usual_reduce_test_() ->
	[{"empty source", ?_assertEqual(1, parallel_reduce:usual_reduce(fun(Item, Agg) -> Item + Agg end, [], 1))},
	 {"integer list", ?_assertEqual(11, parallel_reduce:usual_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3, 4], 1))},
	 {"string list", ?_assertEqual("++aabb", parallel_reduce:usual_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb"], "++"))}].

portion_reduce_test_() ->
	[{"empty source", ?_assertEqual(1, parallel_reduce:portion_reduce(fun(Item, Agg) -> Item + Agg end, [], {1, 0}, 2))},
	 portion_reduce_integer_list(),
	 portion_reduce_string_list()].

portion_reduce_integer_list() -> 
	[{"integer list with small list size", ?_assertEqual(4, parallel_reduce:portion_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2], {1, 0}, 4))},
	 {"integer list with with size equals portion size", ?_assertEqual(7, parallel_reduce:portion_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 3))},
	 {"integer list with with multiple portion", ?_assertEqual(11, parallel_reduce:portion_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3, 4], {1, 0}, 2))},
	 {"integer list with with nonmultiple portion", ?_assertEqual(7, parallel_reduce:portion_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 2))}].

portion_reduce_string_list() -> 
	[{"string list with small list size", ?_assertEqual("++aabb", parallel_reduce:portion_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb"], {"++", ""}, 4))},
	 {"string list with with size equals portion size", ?_assertEqual("++aabbcc", parallel_reduce:portion_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 3))},
	 {"string list with with multiple portion", ?_assertEqual("++aabbccdd", parallel_reduce:portion_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc", "dd"], {"++", ""}, 2))},
	 {"string list with with nonmultiple portion", ?_assertEqual("++aabbcc", parallel_reduce:portion_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 2))}].

portion_gen_reduce_test_() ->
	[{"empty source", ?_assertEqual(1, parallel_reduce:portion_gen_reduce(fun(Item, Agg) -> Item + Agg end, [], {1, 0}, 2))},
	 portion_gen_reduce_integer_list(),
	 portion_gen_reduce_string_list()].

portion_gen_reduce_integer_list() -> 
	[{"integer list with small list size", ?_assertEqual(4, parallel_reduce:portion_gen_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2], {1, 0}, 4))},
	 {"integer list with with size equals portion size", ?_assertEqual(7, parallel_reduce:portion_gen_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 3))},
	 {"integer list with with multiple portion", ?_assertEqual(11, parallel_reduce:portion_gen_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3, 4], {1, 0}, 2))},
	 {"integer list with with nonmultiple portion", ?_assertEqual(7, parallel_reduce:portion_gen_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 2))}].

portion_gen_reduce_string_list() -> 
	[{"string list with small list size", ?_assertEqual("++aabb", parallel_reduce:portion_gen_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb"], {"++", ""}, 4))},
	 {"string list with with size equals portion size", ?_assertEqual("++aabbcc", parallel_reduce:portion_gen_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 3))},
	 {"string list with with multiple portion", ?_assertEqual("++aabbccdd", parallel_reduce:portion_gen_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc", "dd"], {"++", ""}, 2))},
	 {"string list with with nonmultiple portion", ?_assertEqual("++aabbcc", parallel_reduce:portion_gen_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 2))}].

limited_reduce_test_() ->
	[{"empty source", ?_assertEqual(1, parallel_reduce:limited_reduce(fun(Item, Agg) -> Item + Agg end, [], {1, 0}, 2, 2))},
	 limited_reduce_integer_list(),
	 limited_reduce_string_list()].

limited_reduce_integer_list() ->
	[{"integer list with small list size", ?_assertEqual(4, parallel_reduce:limited_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2], {1, 0}, 4, 2))},
	 {"integer list with with size equals portion size", ?_assertEqual(7, parallel_reduce:limited_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 3, 2))},
	 {"integer list with with multiple portion", ?_assertEqual(11, parallel_reduce:limited_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3, 4], {1, 0}, 2, 2))},
	 {"integer list with with nonmultiple portion", ?_assertEqual(7, parallel_reduce:limited_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 2, 2))},
	 {"integer list with with one worker", ?_assertEqual(7, parallel_reduce:limited_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 2, 1))}].

limited_reduce_string_list() ->
	[{"string list with small list size", ?_assertEqual("++aabb", parallel_reduce:limited_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb"], {"++", ""}, 4, 2))},
	 {"string list with with size equals portion size", ?_assertEqual("++aabbcc", parallel_reduce:limited_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 3, 2))},
	 {"string list with with multiple portion", ?_assertEqual("++aabbccdd", parallel_reduce:limited_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc", "dd"], {"++", ""}, 2, 2))},
	 {"string list with with nonmultiple portion", ?_assertEqual("++aabbcc", parallel_reduce:limited_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 2, 2))},
	 {"string list with with one worker", ?_assertEqual("++aabbcc", parallel_reduce:limited_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 2, 1))}].

distributed_reduce_test_() ->
	[{"empty source", ?_assertEqual(1, parallel_reduce:distributed_reduce(fun(Item, Agg) -> Item + Agg end, [], {1, 0}, 2, [node(), node()], 2))},
	 distributed_reduce_integer_list(),
	 distributed_reduce_string_list()].

distributed_reduce_integer_list() ->
	[{"integer list with small list size", ?_assertEqual(4, parallel_reduce:distributed_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2], {1, 0}, 4, [node()], 2))},
	 {"integer list with with size equals portion size", ?_assertEqual(7, parallel_reduce:distributed_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 3, [node()], 2))},
	 {"integer list with with multiple portion", ?_assertEqual(11, parallel_reduce:distributed_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3, 4], {1, 0}, 2, [node()], 2))},
	 {"integer list with with nonmultiple portion", ?_assertEqual(7, parallel_reduce:distributed_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 2, [node()], 2))},
	 {"integer list with with one worker", ?_assertEqual(7, parallel_reduce:distributed_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3], {1, 0}, 2, [node()], 1))},
	 {"integer list with with several nodes", ?_assertEqual(11, parallel_reduce:distributed_reduce(fun(Item, Agg) -> Item + Agg end, [1, 2, 3, 4], {1, 0}, 2, [node(), node()], 2))}].

distributed_reduce_string_list() ->
	[{"string list with small list size", ?_assertEqual("++aabb", parallel_reduce:distributed_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb"], {"++", ""}, 4, [node()], 2))},
	 {"string list with with size equals portion size", ?_assertEqual("++aabbcc", parallel_reduce:distributed_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 3, [node()], 2))},
	 {"string list with with multiple portion", ?_assertEqual("++aabbccdd", parallel_reduce:distributed_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc", "dd"], {"++", ""}, 2, [node()], 2))},
	 {"string list with with nonmultiple portion", ?_assertEqual("++aabbcc", parallel_reduce:distributed_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 2, [node()], 2))},
	 {"string list with with one worker", ?_assertEqual("++aabbcc", parallel_reduce:distributed_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc"], {"++", ""}, 2, [node()], 1))},
	 {"string list with with several nodes", ?_assertEqual("++aabbccdd", parallel_reduce:distributed_reduce(fun(Item, Agg) -> Agg ++ Item end, ["aa", "bb", "cc", "dd"], {"++", ""}, 2, [node(), node()], 2))}].