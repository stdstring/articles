-module(data_generator).

-export([generate_int_data/2, generate_str_data/2, generate_str_data_from_dictionary/2]).

generate_int_data(N, Count) ->
	init_random(),
	[random:uniform(N) || _ <- lists:seq(1, Count)].

generate_str_data(N, Count) ->
	init_random(),
	[generate_random_str(N) || _ <- lists:seq(1, Count)].

generate_str_data_from_dictionary(Dictionary, Count) ->
	init_random(),
	DictionaryLength = length(Dictionary),
	[lists:nth(random:uniform(DictionaryLength), Dictionary) || _ <- lists:seq(1, Count)].

init_random() ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3).

generate_random_str(N) ->
	Alphabet = generate_alphabet(),
	AlphabetLength = length(Alphabet),
	[lists:nth(random:uniform(AlphabetLength), Alphabet) || _ <- lists:seq(1, N)].

generate_alphabet() ->
	generate_alphabet_subset($A, 26) ++ generate_alphabet_subset($a, 26) ++ generate_alphabet_subset($0, 10).

generate_alphabet_subset(StartChar, Count) ->
	lists:map(fun(Index) -> StartChar + Index end, lists:seq(0, Count-1)).
