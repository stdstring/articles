-module(problem_035).
-export([solve/1]).

solve(MaxNumber) ->
	PrimeNumbers = eratos_sieve:get_primes(MaxNumber),
	sets:size(find_circular_primes(PrimeNumbers)).

find_circular_primes(PrimeNumbers) ->
	find_circular_primes(PrimeNumbers, sets:from_list(PrimeNumbers), sets:new()).

find_circular_primes([], _PrimeSet, CircularPrimeSet) -> CircularPrimeSet;
find_circular_primes([PrimeNumber | Rest], PrimeSet, CircularPrimeSet) ->
	case sets:is_element(PrimeNumber, CircularPrimeSet) of
		false ->
			CircularNumbers = create_circular_numbers(PrimeNumber),
			NewCircularPrimeSet = check_and_add_numbers(CircularNumbers, PrimeSet, CircularPrimeSet),
			find_circular_primes(Rest, PrimeSet, NewCircularPrimeSet);
		true -> find_circular_primes(Rest, PrimeSet, CircularPrimeSet)
	end.

check_and_add_numbers(Numbers, PrimeSet, CircularPrimeSet) ->
	Check = lists:all(fun(Number) -> sets:is_element(Number, PrimeSet) end, Numbers),
	if
		Check == true -> lists:foldl(fun(Number, Dest) -> sets:add_element(Number, Dest) end, CircularPrimeSet, Numbers);
		Check == false -> CircularPrimeSet
	end.

create_circular_numbers(Number) ->
	lists:map(fun(Digits) -> get_number(Digits) end, get_circular_shifts(get_digits(Number))).

get_digits(Number) -> [Char-$0 || Char <- integer_to_list(Number)].

get_number(Digits) -> list_to_integer([Digit+$0 || Digit <- Digits]).

get_circular_shifts(Source) ->
	get_circular_shifts(Source, Source, []).

get_circular_shifts(Source, Source, []) ->
	[Head | Tail] = Source,
	get_circular_shifts(Source, Tail ++ [Head], [Source]);
get_circular_shifts(Source, Source, Dest) -> Dest;
get_circular_shifts(Source, Current, Dest) ->
	[Head | Tail] = Current,
	get_circular_shifts(Source, Tail ++ [Head], [Current] ++ Dest).