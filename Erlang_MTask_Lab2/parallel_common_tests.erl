-module(parallel_common_tests).

-include_lib("eunit/include/eunit.hrl").

calc_portion_count_test_() ->
	[{"TotalSize is multiple of PortionSize", ?_assertEqual(3, parallel_common:calc_portion_count(6, 2))},
	 {"TotalSize isn't multiple of PortionSize", ?_assertEqual(4, parallel_common:calc_portion_count(7, 2))}].

prepare_data_test_() ->
	[{"Prepare for empty source", ?_assertEqual([], parallel_common:prepare_data(2, []))},
	 {"Prepare for single element's source", ?_assertEqual([{0, [1]}], parallel_common:prepare_data(2, [1]))},
	 {"Prepare for source of multiple length", ?_assertEqual([{0, [1, 2]}, {1, [3, 4]}], parallel_common:prepare_data(2, [1, 2, 3, 4]))},
	 {"Prepare for source of nonmultiple length", ?_assertEqual([{0, [1, 2]}, {1, [3]}], parallel_common:prepare_data(2, [1, 2, 3]))}].