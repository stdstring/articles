-module(parallel_map).

-export([usual_map/2, simple_pmap/2, portion_pmap/3, portion_gen_pmap/3, limited_pmap/4, distributed_pmap/5]).

%% usual map
usual_map(_Fun, []) -> [];
usual_map(Fun, SourceList) -> [Fun(Element) || Element <- SourceList].

%% simple parallel map
simple_pmap(_Fun, []) -> [];
simple_pmap(Fun, SourceList) ->
	%% lib:flush_receive(),
	process_flag(trap_exit, true),
	MasterPid = self(),
	ElementCount = length(SourceList),
	PreparedData = simple_prepare_data(SourceList),
	%% WorkerList = ...
	lists:foreach(fun({Index, Element}) -> spawn_link(fun() -> simple_worker(Fun, Element, Index, MasterPid) end) end, PreparedData),
	EmptyStorage = array:new([{size, ElementCount}, {fixed, true}, {default, none}]),
	FullStorage = parallel_common:collect_result(EmptyStorage, ElementCount),
	process_flag(trap_exit, false),
	array:to_list(FullStorage).

simple_worker(Fun, SourceElement, Index, MasterPid) ->
	DestElement = Fun(SourceElement),
	MasterPid ! {result, Index, DestElement}.

simple_prepare_data([]) -> [];
simple_prepare_data(SourceList) -> simple_prepare_data(0, SourceList, []).

simple_prepare_data(Index, [Element], PreparedData) -> lists:reverse([{Index, Element}] ++ PreparedData);
simple_prepare_data(Index, [Element|Rest], PreparedData) ->
	simple_prepare_data(Index + 1, Rest, [{Index, Element}] ++ PreparedData).

%% portioned parallel map
portion_pmap(_Fun, [], _PortionSize) -> [];
portion_pmap(Fun, SourceList, PortionSize) when length(SourceList) =< PortionSize -> lists:map(Fun, SourceList);
portion_pmap(Fun, SourceList, PortionSize) ->
	%% lib:flush_receive(),
	process_flag(trap_exit, true),
	MasterPid = self(),
	PortionCount = parallel_common:calc_portion_count(length(SourceList), PortionSize),
	PreparedData = parallel_common:prepare_data(PortionSize, SourceList),
	%% WorkerList = ...
	lists:foreach(fun({Index, Portion}) -> spawn_link(fun() -> portion_worker(Fun, Portion, Index, MasterPid) end) end, PreparedData),
	EmptyStorage = array:new([{size, PortionCount}, {fixed, true}, {default, none}]),
	FullStorage = parallel_common:collect_result(EmptyStorage, PortionCount),
	process_flag(trap_exit, false),
	lists:append(array:to_list(FullStorage)).

portion_worker(Fun, SourcePortion, Index, MasterPid) ->
	DestPortion = lists:map(Fun, SourcePortion),
	MasterPid ! {result, Index, DestPortion}.

%% portioned generalized parallel map
portion_gen_pmap(_Fun, [], _PortionSize) -> [];
portion_gen_pmap(Fun, SourceList, PortionSize) when length(SourceList) =< PortionSize -> lists:map(Fun, SourceList);
portion_gen_pmap(Fun, SourceList, PortionSize) ->
	WorkerFun = fun(SourcePortion) -> lists:map(Fun, SourcePortion) end,
	parallel_portion_helper:portion_core(WorkerFun, fun lists:append/1, SourceList, PortionSize).

%% limited parallel map
limited_pmap(_Fun, [], _PortionSize, _WorkerCount) -> [];
limited_pmap(Fun, SourceList, PortionSize, _WorkerCount) when length(SourceList) =< PortionSize -> lists:map(Fun, SourceList);
limited_pmap(Fun, SourceList, PortionSize, WorkerCount) ->
	WorkerFun = fun(SourcePortion) -> lists:map(Fun, SourcePortion) end,
	WorkerList = [spawn_link(fun() -> parallel_limited_helper:limited_worker(WorkerFun) end) || _WorkerIndex <- lists:seq(1, WorkerCount)],
	Result = parallel_limited_helper:limited_core(fun lists:append/1, SourceList, PortionSize, WorkerList),
	lists:foldl(fun(Worker, _Aggr) -> exit(Worker, normal) end, true, WorkerList),
	Result.

%% distributed parallel map
distributed_pmap(_Fun, [], _PortionSize, _NodeList, _NodeWorkerCount) -> [];
distributed_pmap(Fun, SourceList, PortionSize, _NodeList, _NodeWorkerCount) when length(SourceList) =< PortionSize ->
	lists:map(Fun, SourceList);
distributed_pmap(Fun, SourceList, PortionSize, NodeList, NodeWorkerCount) ->
	WorkerFun = fun(SourcePortion) -> lists:map(Fun, SourcePortion) end,
	WorkerList = [spawn_link(Node, fun() -> parallel_limited_helper:limited_worker(WorkerFun) end) || Node <- NodeList, _WorkerIndex <- lists:seq(1, NodeWorkerCount)],
	Result = parallel_limited_helper:limited_core(fun lists:append/1, SourceList, PortionSize, WorkerList),
	lists:foldl(fun(Worker, _Aggr) -> exit(Worker, normal) end, true, WorkerList),
	Result.