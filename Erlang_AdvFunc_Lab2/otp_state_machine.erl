-module(otp_state_machine).

-record(internal_state, {init_state, module_name}).
-include("state.hrl").

-export([build/2, start/2, send/2]).

build(InitStateId, ModuleName) ->
	#internal_state{init_state = InitStateId, module_name = ModuleName}.

start(InitStateData, InternalState) ->
	#state{state_id = InternalState#internal_state.init_state, state_data = InitStateData, internal_state = InternalState}.
	
send({EventId, EventData}, State) ->
	InternalState = State#state.internal_state,
	Args = [{EventId, EventData}, {State#state.state_id, State#state.state_data}],
	{NewStateId, NewStateData} = apply(InternalState#internal_state.module_name, State#state.state_id, Args),
	State#state{state_id = NewStateId, state_data = NewStateData}.