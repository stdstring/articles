-module(simple_message_handler).

-export([message_loop/0, message_loop/1, special_message_loop/0]).

message_loop() ->
	receive
		Message -> io:format("Message: ~p~n", [Message]),
		message_loop()
	end.

message_loop(State) ->
	receive
		{m1, SenderPid} ->
			{ReturnValue, NewState} = some_module:func1(State),
			SenderPid ! {r1, ReturnValue},
			message_loop(NewState);
		{m2, SenderPid} ->
			ReturnValue = some_module:func2(State),
			SenderPid ! {r2, ReturnValue},
			message_loop(State)
	end.

special_message_loop() ->
	receive
		m1 ->
			io:format("m1 !!! ~n", []),
			special_message_loop();
		m2 ->
			io:format("m2 !!! ~n", []),
			special_message_loop()
	end.