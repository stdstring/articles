-module(parallel_reduce).

-export([usual_reduce/3, portion_reduce/4, portion_gen_reduce/4, limited_reduce/5, distributed_reduce/6, smartmsg_reduce/5, pool_reduce/6]).

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
	MasterPid = self(),
	PortionCount = parallel_common:calc_portion_count(length(SourceList), PortionSize),
	PreparedData = parallel_common:prepare_data(PortionSize, SourceList),
	lists:foreach(fun({Index, Portion}) -> spawn_link(fun() -> portion_worker(Fun, Portion, PortionInitValue, Index, MasterPid) end) end, PreparedData),
	EmptyStorage = array:new([{size, PortionCount}, {fixed, true}, {default, none}]),
	FullStorage = parallel_common:collect_result(EmptyStorage, PortionCount),
	process_flag(trap_exit, false),
	lists:foldl(Fun, InitValue, array:to_list(FullStorage)).

portion_worker(Fun, SourcePortion, InitValue, Index, MasterPid)->
	AggrValue = lists:foldl(Fun, InitValue, SourcePortion),
	MasterPid ! {result, Index, AggrValue}.

%% portioned generalized parallel reduce
portion_gen_reduce(_Fun, [], {InitValue, _PortionInitValue}, _PortionSize) -> InitValue;
portion_gen_reduce(Fun, SourceList, {InitValue, _PortionInitValue}, PortionSize) when length(SourceList) =< PortionSize ->
	lists:foldl(Fun, InitValue, SourceList);
portion_gen_reduce(Fun, SourceList, {InitValue, PortionInitValue}, PortionSize) ->
	ReduceFun = fun(List) -> lists:foldl(Fun, InitValue, List) end,
	PortionReduceFun = fun(List) -> lists:foldl(Fun, PortionInitValue, List) end,
	parallel_portion_helper:portion_core(PortionReduceFun, ReduceFun, SourceList, PortionSize).

%% limited parallel reduce
limited_reduce(_Fun, [], {InitValue, _PortionInitValue}, _PortionSize, _WorkerCount) -> InitValue;
limited_reduce(Fun, SourceList, {InitValue, _PortionInitValue}, PortionSize, _WorkerCount) when length(SourceList) =< PortionSize ->
	lists:foldl(Fun, InitValue, SourceList);
limited_reduce(Fun, SourceList, {InitValue, PortionInitValue}, PortionSize, WorkerCount) ->
	ReduceFun = fun(List) -> lists:foldl(Fun, InitValue, List) end,
	PortionReduceFun = fun(List) -> lists:foldl(Fun, PortionInitValue, List) end,
	WorkerList = [spawn_link(fun() -> parallel_limited_helper:limited_worker(PortionReduceFun) end) || _WorkerIndex <- lists:seq(1, WorkerCount)],
	Result = parallel_limited_helper:limited_core(ReduceFun, SourceList, PortionSize, WorkerList),
	lists:foldl(fun(Worker, _Aggr) -> exit(Worker, normal) end, true, WorkerList),
	Result.

%% distributed parallel reduce
distributed_reduce(_Fun, [], {InitValue, _PortionInitValue}, _PortionSize, _NodeList, _NodeWorkerCount) -> InitValue;
distributed_reduce(Fun, SourceList, {InitValue, _PortionInitValue}, PortionSize, _NodeList, _NodeWorkerCount) when length(SourceList) =< PortionSize ->
	lists:foldl(Fun, InitValue, SourceList);
distributed_reduce(Fun, SourceList, {InitValue, PortionInitValue}, PortionSize, NodeList, NodeWorkerCount) ->
	ReduceFun = fun(List) -> lists:foldl(Fun, InitValue, List) end,
	PortionReduceFun = fun(List) -> lists:foldl(Fun, PortionInitValue, List) end,
	WorkerList = [spawn_link(Node, fun() -> parallel_limited_helper:limited_worker(PortionReduceFun) end) || Node <- NodeList, _WorkerIndex <- lists:seq(1, NodeWorkerCount)],
	Result = parallel_limited_helper:limited_core(ReduceFun, SourceList, PortionSize, WorkerList),
	lists:foldl(fun(Worker, _Aggr) -> exit(Worker, normal) end, true, WorkerList),
	Result.

%% smart messaging parallel reduce
smartmsg_reduce(_Fun, [], {InitValue, _PortionInitValue}, _PortionSize, _WorkerCount) -> InitValue;
smartmsg_reduce(Fun, SourceList, {InitValue, _PortionInitValue}, PortionSize, _WorkerCount) when length(SourceList) =< PortionSize ->
	lists:foldl(Fun, InitValue, SourceList);
smartmsg_reduce(Fun, SourceList, {InitValue, PortionInitValue}, PortionSize, WorkerCount) ->
	ReduceFun = fun(List) -> lists:foldl(Fun, InitValue, List) end,
	PortionReduceFun = fun(List) -> lists:foldl(Fun, PortionInitValue, List) end,
	WorkerList = [spawn_link(fun() -> parallel_smartmsg_helper:smartmsg_worker(PortionReduceFun) end) || _WorkerIndex <- lists:seq(1, WorkerCount)],
	Result = parallel_smartmsg_helper:smartmsg_core(ReduceFun, SourceList, PortionSize, WorkerList),
	lists:foldl(fun(Worker, _Aggr) -> exit(Worker, normal) end, true, WorkerList),
	Result.

%% pooling parallel reduce
pool_reduce(Fun, SourceList, {InitValue, PortionInitValue}, PortionSize, WorkerCount, PoolNodes) ->
	pool_helper:start_pool(pmap, PoolNodes),
	Result = pool_reduce_impl(Fun, SourceList, {InitValue, PortionInitValue}, PortionSize, WorkerCount),
	pool_helper:stop_pool(),
	Result.

pool_reduce_impl(_Fun, [], {InitValue, _PortionInitValue}, _PortionSize, _WorkerCount) -> InitValue;
pool_reduce_impl(Fun, SourceList, {InitValue, _PortionInitValue}, PortionSize, _WorkerCount) when length(SourceList) =< PortionSize ->
	lists:foldl(Fun, InitValue, SourceList);
pool_reduce_impl(Fun, SourceList, {InitValue, PortionInitValue}, PortionSize, WorkerCount) ->
	WorkerFun = fun(Portion) -> lists:foldl(Fun, PortionInitValue, Portion) end,
	AssignFun = fun(Portion, Index, Master) -> pool:pspawn_link(parallel_pool_helper, pool_worker, [WorkerFun, Portion, Index, Master]) end,
	FinalAggFun = fun(List) -> lists:foldl(Fun, InitValue, List) end,
	parallel_pool_helper:pool_core(FinalAggFun, SourceList, PortionSize, AssignFun, WorkerCount).