-module(parallel_common).

-export([calc_portion_count/2, prepare_data/2, collect_result/2]).

calc_portion_count(TotalSize, PortionSize) when TotalSize rem PortionSize == 0 -> TotalSize div PortionSize;
calc_portion_count(TotalSize, PortionSize) when TotalSize rem PortionSize /= 0 -> (TotalSize div PortionSize) + 1.

prepare_data(_PortionSize, []) -> [];
prepare_data(PortionSize, SourceList) -> prepare_data(0, PortionSize, SourceList, []).

prepare_data(Index, PortionSize, SourceList, PreparedData) when length(SourceList) =< PortionSize ->
	lists:reverse([{Index, SourceList}] ++ PreparedData);
prepare_data(Index, PortionSize, SourceList, PreparedData) ->
	{Portion, Rest} = lists:split(PortionSize, SourceList),
	prepare_data(Index + 1, PortionSize, Rest, [{Index, Portion}] ++ PreparedData).

collect_result(ResultStorage, TotalCount) -> collect_result(ResultStorage, TotalCount, 0).

collect_result(ResultStorage, TotalCount, TotalCount) -> ResultStorage; 
collect_result(ResultStorage, TotalCount, ProcessedCount) ->
	receive
		{'EXIT', _From, normal} -> collect_result(ResultStorage, TotalCount, ProcessedCount);
		{'EXIT', _From, Reason} -> error({internal_error, Reason});
		{result, Index, DestElement} ->
			UpdatedResultStorage = array:set(Index, DestElement, ResultStorage),
			collect_result(UpdatedResultStorage, TotalCount, ProcessedCount + 1);
		_Other -> collect_result(ResultStorage, TotalCount, ProcessedCount)
	end.