-module(parallel_portion_helper).

-export([portion_core/4]).

portion_core(WorkerFun, FinalAggrFun, SourceList, PortionSize) ->
	process_flag(trap_exit, true),
	MasterPid = self(),
	PortionCount = parallel_common:calc_portion_count(length(SourceList), PortionSize),
	PreparedData = parallel_common:prepare_data(PortionSize, SourceList),
	%% WorkerList = ...
	lists:foreach(fun({Index, Portion}) -> spawn_link(fun() -> portion_worker(WorkerFun, Portion, Index, MasterPid) end) end, PreparedData),
	EmptyStorage = array:new([{size, PortionCount}, {fixed, true}, {default, none}]),
	FullStorage = parallel_common:collect_result(EmptyStorage, PortionCount),
	process_flag(trap_exit, false),
	FinalAggrFun(array:to_list(FullStorage)).

portion_worker(Fun, SourcePortion, Index, MasterPid) ->
	DestPortion = Fun(SourcePortion),
	MasterPid ! {result, Index, DestPortion}.