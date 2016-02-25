-module(parallel_reduce_demo).

-export([reduce_demo_fun/2]).

reduce_demo_fun(Number, Sum) ->
    Sum + math:sqrt(Number).