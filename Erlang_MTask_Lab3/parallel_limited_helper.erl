-module(parallel_limited_helper).

-export([limited_core/4, limited_worker/1]).

limited_core(FinalAggrFun, SourceList, PortionSize, WorkerList) ->
	%% lib:flush_receive(),
	process_flag(trap_exit, true),
	PortionCount = parallel_common:calc_portion_count(length(SourceList), PortionSize),
	PreparedData = parallel_common:prepare_data(PortionSize, SourceList),
	send_worker_tasks(PreparedData, WorkerList),
	EmptyStorage = array:new([{size, PortionCount}, {fixed, true}, {default, none}]),
	FullStorage = parallel_common:collect_result(EmptyStorage, PortionCount),
	% lists:foldl(fun(Worker, _Aggr) -> exit(Worker, normal) end, true, WorkerList),
	process_flag(trap_exit, false),
	FinalAggrFun(array:to_list(FullStorage)).

send_worker_tasks(PreparedData, WorkerList) -> send_worker_tasks(PreparedData, WorkerList, 1).

send_worker_tasks([], _WorkerList, _WorkerIndex) -> complete;
send_worker_tasks(PreparedData, WorkerList, WorkerIndex) when WorkerIndex > length(WorkerList) ->
	send_worker_tasks(PreparedData, WorkerList, 1);
send_worker_tasks([{Index, Portion} | Rest], WorkerList, WorkerIndex) ->
	Worker = lists:nth(WorkerIndex, WorkerList),
	Worker ! {task_request, self(), Index, Portion},
	send_worker_tasks(Rest, WorkerList, WorkerIndex + 1).

limited_worker(Fun) ->
	receive
		{task_request, MasterPid, Index, SourcePortion} ->
			Dest = Fun(SourcePortion),
			MasterPid ! {result, Index, Dest},
			limited_worker(Fun);
		_Other -> limited_worker(Fun)
	end.