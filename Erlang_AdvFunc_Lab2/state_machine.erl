-module(state_machine).

-record(internal_state, {init_state, transition_table = dict:new(), state_set = sets:new()}).
-include("state.hrl").

-export([build_init/1, build_transition/3, build_terminal/2, start/2, send/2]).

build_init(InitStateId) ->
	#internal_state{init_state = InitStateId, state_set = sets:add_element(InitStateId, sets:new())}.

build_transition({EventId, StateId}, Action, InternalState) ->
	Table = InternalState#internal_state.transition_table,
	StateSet = InternalState#internal_state.state_set,
	case dict:is_key({EventId, StateId}, Table) of
		true -> erlang:error(build_error);
		false ->
			UpdatedTable = dict:store({EventId, StateId}, Action, Table),
			UpdatedStateSet = sets:add_element(StateId, StateSet),
			InternalState#internal_state{transition_table = UpdatedTable, state_set = UpdatedStateSet}
	end.
	
build_terminal(TerminalStateId, InternalState) ->
	StateSet = InternalState#internal_state.state_set,
	InternalState#internal_state{state_set = sets:add_element(TerminalStateId, StateSet)}.
	
start(InitStateData, InternalState) ->
	#state{state_id = InternalState#internal_state.init_state, state_data = InitStateData, internal_state = InternalState}.
	
send({EventId, EventData}, State) ->
	InternalState = State#state.internal_state,
	Table = InternalState#internal_state.transition_table,
	case dict:find({EventId, State#state.state_id}, Table) of
		{ok, Action} -> process_event({EventId, EventData}, State, Action);
		error -> erlang:error(bad_event)
	end.
	
process_event({EventId, EventData}, State, Action) ->
	{NewStateId, NewStateData} = Action({EventId, EventData}, {State#state.state_id, State#state.state_data}),
	InternalState = State#state.internal_state,
	StateSet = InternalState#internal_state.state_set,
	case sets:is_element(NewStateId, StateSet) of
		true -> State#state{state_id = NewStateId, state_data = NewStateData};
		false -> erlang:error(bad_state)
	end.