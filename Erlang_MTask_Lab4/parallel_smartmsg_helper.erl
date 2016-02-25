-module(parallel_smartmsg_helper).

-export([smartmsg_core/4, smartmsg_worker/1]).

-record(tasks_descr, {created = 0, processed = 0, rest = []}).
-record(task_request, {master, index, portion}).
-record(task_result, {worker, index, result}).

smartmsg_core(FinalAggrFun, SourceList, PortionSize, WorkerList) ->
	process_flag(trap_exit, true),
	TasksDescr = distribute_init_tasks(#tasks_descr{rest = SourceList}, PortionSize, WorkerList),
	PortionCount = parallel_common:calc_portion_count(length(SourceList), PortionSize),
	EmptyStorage = array:new([{size, PortionCount}, {fixed, true}, {default, none}]),
	FullStorage = handle_workers(TasksDescr, EmptyStorage, PortionSize),
	process_flag(trap_exit, false),
	FinalAggrFun(array:to_list(FullStorage)).

distribute_init_tasks(#tasks_descr{created = Created, rest = []}, _PortionSize, _WorkerList) ->
	#tasks_descr{created = Created, rest = []};
distribute_init_tasks(TasksDescr, _PortionSize, []) -> TasksDescr;
distribute_init_tasks(#tasks_descr{created = Created, rest = Source}, PortionSize, [Worker | WorkerRest]) ->
	Rest = assign_task(Worker, Source, PortionSize, Created),
	TasksDescr = #tasks_descr{created = Created + 1, rest = Rest},
	distribute_init_tasks(TasksDescr, PortionSize, WorkerRest).

handle_workers(#tasks_descr{created = Number, processed = Number, rest = []}, Storage, _PortionSize) ->
	Storage;
handle_workers(#tasks_descr{created = Created, processed = Processed, rest = []}, Storage, PortionSize) ->
	receive
		#task_result{index = Index, result = Dest} ->
			UpdatedStorage = collect_result(Dest, Index, Storage),
			TasksDescr = #tasks_descr{created = Created, processed = Processed + 1, rest = []},
			handle_workers(TasksDescr, UpdatedStorage, PortionSize);
		_Other -> handle_workers(#tasks_descr{created = Created, processed = Processed, rest = []}, Storage, PortionSize)
	end;
handle_workers(#tasks_descr{created = Created, processed = Processed, rest = Source}, Storage, PortionSize) ->
	receive
		#task_result{worker = Worker, index = Index, result = Dest} ->
			UpdatedStorage = collect_result(Dest, Index, Storage),
			Rest = assign_task(Worker, Source, PortionSize, Created),
			TasksDescr = #tasks_descr{created = Created + 1, processed = Processed + 1, rest = Rest},
			handle_workers(TasksDescr, UpdatedStorage, PortionSize);
		_Other -> handle_workers(#tasks_descr{created = Created, processed = Processed, rest = Source}, Storage, PortionSize)
	end.

collect_result(Result, Index, Storage) ->
	array:set(Index, Result, Storage).

assign_task(Worker, SourceList, PortionSize, Index) when length(SourceList) =< PortionSize ->
	Worker ! #task_request{master = self(), index = Index, portion = SourceList},
	[];
assign_task(Worker, SourceList, PortionSize, Index) ->
	{Portion, Rest} = lists:split(PortionSize, SourceList),
	Worker ! #task_request{master = self(), index = Index, portion = Portion},
	Rest.

smartmsg_worker(Fun) ->
	receive
		#task_request{master = MasterPid, index = Index, portion = SourcePortion} ->
			Dest = Fun(SourcePortion),
			MasterPid ! #task_result{worker = self(), index = Index, result = Dest},
			smartmsg_worker(Fun);
		_Other -> smartmsg_worker(Fun)
	end.