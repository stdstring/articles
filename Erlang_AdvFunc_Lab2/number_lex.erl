-module(number_lex).

-include("state.hrl").
-include("state_data.hrl").

-export([build_intlex_fsm/0, build_intlex_otpfsm/0, build_reallex_fsm/0, build_reallex_otpfsm/0]).
-export([process/3]).

build_intlex_fsm() ->
	IntState0 = state_machine:build_init(start),
	IntState1 = state_machine:build_transition({char, start}, fun integer_fsm:start/2, IntState0),
	IntState2 = state_machine:build_transition({char, sign}, fun integer_fsm:sign/2, IntState1),
	IntState3 = state_machine:build_transition({char, digit}, fun integer_fsm:digit/2, IntState2),
	state_machine:build_terminal(unrecognized, IntState3).

build_intlex_otpfsm() -> otp_state_machine:build(start, integer_fsm).

build_reallex_fsm() ->
	IntState0 = state_machine:build_init(start),
	IntState1 = state_machine:build_transition({char, start}, fun real_fsm:start/2, IntState0),
	IntState2 = state_machine:build_transition({char, int_part_sign}, fun real_fsm:int_part_sign/2, IntState1),
	IntState3 = state_machine:build_transition({char, int_part_digit}, fun real_fsm:int_part_digit/2, IntState2),
	IntState4 = state_machine:build_transition({char, fract_part_separator}, fun real_fsm:fract_part_separator/2, IntState3),
	IntState5 = state_machine:build_transition({char, fract_part_digit}, fun real_fsm:fract_part_digit/2, IntState4),
	IntState6 = state_machine:build_transition({char, exp_part_separator}, fun real_fsm:exp_part_separator/2, IntState5),
	IntState7 = state_machine:build_transition({char, exp_part_sign}, fun real_fsm:exp_part_sign/2, IntState6),
	IntState8 = state_machine:build_transition({char, exp_part_digit}, fun real_fsm:exp_part_digit/2, IntState7),
	state_machine:build_terminal(unrecognized, IntState8).

build_reallex_otpfsm() -> otp_state_machine:build(start, real_fsm).

process(Source, FSMModule, InternalState) -> process_impl(Source, FSMModule, apply(FSMModule, start, [#state_data{}, InternalState])).

process_impl("", _FSMModule, State) ->
	StateData = State#state.state_data,
	{StateData#state_data.recognized, string:sub_string(StateData#state_data.pull, length(StateData#state_data.recognized) + 1)};
process_impl([Char | Rest], FSMModule, State) ->
	case apply(FSMModule, send, [{char, Char}, State]) of
		#state{state_id = unrecognized, state_data = NewStateData} ->
			{NewStateData#state_data.recognized, string:sub_string(NewStateData#state_data.pull, length(NewStateData#state_data.recognized) + 1) ++ [Char] ++ Rest};
		NewState -> process_impl(Rest, FSMModule, NewState)
	end.