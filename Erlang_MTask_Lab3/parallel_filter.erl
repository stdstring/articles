-module(parallel_filter).

-export([portion_gen_filter/3]).

portion_gen_filter(_Fun, [], _PortionSize) -> [];
portion_gen_filter(Fun, SourceList, PortionSize) when length(SourceList) =< PortionSize ->
	lists:filter(Fun, SourceList);
portion_gen_filter(Fun, SourceList, PortionSize) ->
	WorkerFun = fun(SourcePortion) -> lists:filter(Fun, SourcePortion) end,
	parallel_portion_helper:portion_core(WorkerFun, fun lists:append/1, SourceList, PortionSize).