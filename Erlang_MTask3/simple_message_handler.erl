-module(simple_message_handler).

-export([message_loop/0]).

message_loop() ->
	receive
		Message -> io:format("Message: ~p~n", [Message]),
		message_loop()
	end.