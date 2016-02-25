-module(performance_tester).

-export([time_test/3]).

time_test(Fun, TestCount, Count) ->
	TimeValues = time_test(Fun, TestCount, Count, []),
	Mean = calc_mean(TimeValues),
	Deviation = calc_standard_deviation(Mean, TimeValues),
	erlang:garbage_collect(),
	{Mean, Deviation}.

time_test(_Fun, _TestCount, 0, Values) -> Values;
time_test(Fun, TestCount, Count, Values) ->
	Time = time_test(Fun, TestCount),
	erlang:garbage_collect(),
	time_test(Fun, TestCount, Count - 1, [Time] ++ Values).

time_test(Fun, TestCount) ->
	{Time, _Value} = timer:tc(fun() -> time_test_body(Fun, TestCount) end),
	Time / TestCount.

time_test_body(_Fun, 0) -> true;
time_test_body(Fun, TestCount) ->
	Fun(),
	time_test_body(Fun, TestCount - 1).

calc_mean(Values) ->
	lists:sum(Values) / length(Values).

calc_standard_deviation(Mean, Values) ->
	Sum = lists:foldl(fun (Value, Acc) -> Acc + (Value - Mean) * (Value - Mean) end, 0, Values),
	math:sqrt(Sum / length(Values)).
