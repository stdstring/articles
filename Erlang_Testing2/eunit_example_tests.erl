-module(eunit_example_tests).

-include_lib("eunit/include/eunit.hrl").

-define(assertDoesNotException(Expr), ?assertEqual(true, try (_=Expr), true catch _:_ -> false end)).

example1_test() ->
    ?assert(4 == 2*2),
    ?assertNot(5 == 2*2),
    ?assertEqual(4, 2*2),
    ?assertNotEqual(5, 2*2),
    ?assertMatch([1 | _Rest], [1, 2, 3, 4]),
    ?assertMatch([1 | Rest] when length(Rest) == 2;length(Rest) == 3, [1, 2, 3, 4]),
    ?assertNotMatch([2, _Rest], [1, 2, 3, 4]),
    ?assertNotMatch([1 | Rest] when length(Rest) == 1;length(Rest) == 2, [1, 2, 3, 4]),
    ?assertError(some_error_reason, error(some_error_reason)),
    ?assertExit(some_exit_reason, exit(some_exit_reason)),
    ?assertThrow(some_throw_reason, throw(some_throw_reason)),
    ?assertException(error, some_error_reason, error(some_error_reason)),
    ?assertException(exit, some_exit_reason, exit(some_exit_reason)),
    ?assertException(throw, some_throw_reason, throw(some_throw_reason)),
    ?assertNotException(error, _, 2+3),
    ?assertNotException(exit, some_error_reason, error(some_error_reason)),
    ?assertNotException(error, some_exit_reason, error(some_error_reason)),
    ?assertDoesNotException(2+3),
    %% example of bad case
    %% ?assertDoesNotException(error(iddqd)),
    ?assertCmd("echo hello world !"),
    ?assertCmdStatus(127, "some-unknown-command"),
    ?assertCmdOutput("hello world !\n", "echo hello world !"),
    ?assert(?cmd("echo hello world !") /= ""),
    ?debugHere,
    ?debugMsg("Some debug message"),
    ?debugFmt("Some format message: ~p", [{format_data, 666}]),
    ?assertEqual(4, ?debugVal(2*2)),
    ?assertEqual(ok, ?debugTime("some long calculating", timer:sleep(1000))).