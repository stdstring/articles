-module(parallel_reduce).

-export([usual_reduce/3, portion_reduce/4, limited_reduce/5, distributed_reduce/6]).

%% usual_reduce
usual_reduce(_Fun, [], InitValue) -> InitValue;
usual_reduce(Fun, [H | Rest], InitValue) ->
	NewAgg = Fun(H, InitValue),
	usual_reduce(Fun, Rest, NewAgg).

%% portioned parallel reduce
portion_reduce(_Fun, [], {InitValue, _PortionInitValue}, _PortionSize) -> InitValue;
portion_reduce(Fun, SourceList, {InitValue, _PortionInitValue}, PortionSize) when length(SourceList) =< PortionSize ->
	lists:foldl(Fun, InitValue, SourceList);
portion_reduce(Fun, SourceList, {InitValue, PortionInitValue}, PortionSize) ->
	%% lib:flush_receive(),
	process_flag(trap_exit, true),
	ReduceFun = fun(List) -> lists:foldl(Fun, PortionInitValue, List) end,
	MasterPid = self(),
	PortionCount = parallel_common:calc_portion_count(length(SourceList), PortionSize),
	PreparedData = parallel_common:prepare_data(PortionSize, SourceList),
	%% WorkerList = ...
	lists:foreach(fun({Index, Portion}) -> spawn_link(fun() -> portion_worker(ReduceFun, Portion, Index, MasterPid) end) end, PreparedData),
	EmptyStorage = array:new([{size, PortionCount}, {fixed, true}, {default, none}]),
	FullStorage = parallel_common:collect_result(EmptyStorage, PortionCount),
	process_flag(trap_exit, false),
	lists:foldl(Fun, InitValue, array:to_list(FullStorage)).

portion_worker(Fun, SourcePortion, Index, MasterPid)->
	AggrValue = Fun(SourcePortion),
	MasterPid ! {result, Index, AggrValue}.

%% limited parallel reduce
limited_reduce(_Fun, [], {InitValue, _PortionInitValue}, _PortionSize, _WorkerCount) -> InitValue;
limited_reduce(Fun, SourceList, {InitValue, _PortionInitValue}, PortionSize, _WorkerCount) when length(SourceList) =< PortionSize ->
	lists:foldl(Fun, InitValue, SourceList);
limited_reduce(Fun, SourceList, {InitValue, PortionInitValue}, PortionSize, WorkerCount) ->
	ReduceFun = fun(List) -> lists:foldl(Fun, InitValue, List) end,
	PortionReduceFun = fun(List) -> lists:foldl(Fun, PortionInitValue, List) end,
	WorkerList = [spawn_link(fun() -> parallel_limited_helper:limited_worker(PortionReduceFun) end) || _WorkerIndex <- lists:seq(1, WorkerCount)],
	parallel_limited_helper:limited_core(SourceList, PortionSize, WorkerList, ReduceFun).

%% distributed parallel reduce
distributed_reduce(_Fun, [], {InitValue, _PortionInitValue}, _PortionSize, _NodeList, _NodeWorkerCount) -> InitValue;
distributed_reduce(Fun, SourceList, {InitValue, _PortionInitValue}, PortionSize, _NodeList, _NodeWorkerCount) when length(SourceList) =< PortionSize ->
	lists:foldl(Fun, InitValue, SourceList);
distributed_reduce(Fun, SourceList, {InitValue, PortionInitValue}, PortionSize, NodeList, NodeWorkerCount) ->
	ReduceFun = fun(List) -> lists:foldl(Fun, InitValue, List) end,
	PortionReduceFun = fun(List) -> lists:foldl(Fun, PortionInitValue, List) end,
	WorkerList = [spawn_link(Node, fun() -> parallel_limited_helper:limited_worker(PortionReduceFun) end) || Node <- NodeList, _WorkerIndex <- lists:seq(1, NodeWorkerCount)],
	parallel_limited_helper:limited_core(SourceList, PortionSize, WorkerList, ReduceFun).