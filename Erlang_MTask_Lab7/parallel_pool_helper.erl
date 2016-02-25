-module(parallel_pool_helper).

-export([pool_core/5, pool_worker/4]).

-record(tasks_descr, {created = 0, processed = 0, rest = []}).
-record(task_result, {index, result}).

pool_core(FinalAggrFun, SourceList, PortionSize, AssignFun, WorkerCount) ->
    process_flag(trap_exit, true),
    TasksDescr = distribute_init_tasks(#tasks_descr{rest = SourceList}, PortionSize, AssignFun, WorkerCount),
    PortionCount = parallel_common:calc_portion_count(length(SourceList), PortionSize),
    EmptyStorage = array:new([{size, PortionCount}, {fixed, true}, {default, none}]),
    FullStorage = handle_workers(TasksDescr, EmptyStorage, PortionSize, AssignFun),
    process_flag(trap_exit, false),
    FinalAggrFun(array:to_list(FullStorage)).

distribute_init_tasks(#tasks_descr{created = Created, rest = []}, _PortionSize, _AssignFun, _WorkerCount) ->
    #tasks_descr{created = Created, rest = []};
distribute_init_tasks(#tasks_descr{created = WorkerCount, rest = Rest}, _PortionSize, _AssignFun, WorkerCount) ->
    #tasks_descr{created = WorkerCount, rest = Rest};
distribute_init_tasks(#tasks_descr{created = Created, rest = Source}, PortionSize, AssignFun, WorkerCount) ->
    Rest = assign_task(Source, PortionSize, Created, AssignFun),
    TasksDescr = #tasks_descr{created = Created + 1, rest = Rest},
    distribute_init_tasks(TasksDescr, PortionSize, AssignFun, WorkerCount).

handle_workers(#tasks_descr{created = Number, processed = Number, rest = []}, Storage, _PortionSize, _AssignFun) ->
    Storage;
handle_workers(#tasks_descr{created = Created, processed = Processed, rest = []}, Storage, PortionSize, AssignFun) ->
    receive
        #task_result{index = Index, result = Dest} ->
            UpdatedStorage = collect_result(Dest, Index, Storage),
            TasksDescr = #tasks_descr{created = Created, processed = Processed + 1, rest = []},
            handle_workers(TasksDescr, UpdatedStorage, PortionSize, AssignFun);
        _Other -> handle_workers(#tasks_descr{created = Created, processed = Processed, rest = []}, Storage, PortionSize, AssignFun)
    end;
handle_workers(#tasks_descr{created = Created, processed = Processed, rest = Source}, Storage, PortionSize, AssignFun) ->
    receive
        #task_result{index = Index, result = Dest} ->
            UpdatedStorage = collect_result(Dest, Index, Storage),
            Rest = assign_task(Source, PortionSize, Created, AssignFun),
            TasksDescr = #tasks_descr{created = Created + 1, processed = Processed + 1, rest = Rest},
            handle_workers(TasksDescr, UpdatedStorage, PortionSize, AssignFun);
        _Other -> handle_workers(#tasks_descr{created = Created, processed = Processed, rest = Source}, Storage, PortionSize, AssignFun)
    end.

collect_result(Result, Index, Storage) ->
    array:set(Index, Result, Storage).

assign_task(SourceList, PortionSize, Index, AssignFun) when length(SourceList) =< PortionSize ->
    AssignFun(SourceList, Index, self()),
    [];
assign_task(SourceList, PortionSize, Index, AssignFun) ->
    {Portion, Rest} = lists:split(PortionSize, SourceList),
    AssignFun(Portion, Index, self()),
    Rest.

pool_worker(Fun, Portion, Index, Master) ->
    io:format("Current node: ~p~n", [node()]),
    Result = Fun(Portion),
    Master ! #task_result{index = Index, result = Result}.