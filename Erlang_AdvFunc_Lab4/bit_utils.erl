-module(bit_utils).

-export([reverse_bytes/1, reverse_bits/1]).
-export([integer_size/1, integer_signed_size/1, integer_to_binary/3]).
-export([setbit_count/1, bit_parity/1, head_unsetbit_count/1, tail_unsetbit_count/1, inverse/1]).

reverse_bytes(Binary) when is_binary(Binary) ->
	list_to_binary(lists:reverse(binary_to_list(Binary))).

reverse_bits(Bitstring) when is_bitstring(Bitstring) ->
	Segments = bitstring_to_list(Bitstring),
	ReversedSegments = lists:map(fun reverse_segment/1, lists:reverse(Segments)),
	list_to_bitstring(ReversedSegments).

reverse_segment(Number) when is_integer(Number) ->
	reverse_segment(<<Number:8>>);
reverse_segment(<<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>>) ->
	<<B0:1, B1:1, B2:1, B3:1, B4:1, B5:1, B6:1, B7:1>>;
reverse_segment(<<B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>>) ->
	<<B0:1, B1:1, B2:1, B3:1, B4:1, B5:1, B6:1>>;
reverse_segment(<<B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>>) ->
	<<B0:1, B1:1, B2:1, B3:1, B4:1, B5:1>>;
reverse_segment(<<B4:1, B3:1, B2:1, B1:1, B0:1>>) ->
	<<B0:1, B1:1, B2:1, B3:1, B4:1>>;
reverse_segment(<<B3:1, B2:1, B1:1, B0:1>>) ->
	<<B0:1, B1:1, B2:1, B3:1>>;
reverse_segment(<<B2:1, B1:1, B0:1>>) ->
	<<B0:1, B1:1, B2:1>>;
reverse_segment(<<B1:1, B0:1>>) ->
	<<B0:1, B1:1>>;
reverse_segment(<<B0:1>>) ->
	<<B0:1>>.

integer_signed_size(0) -> 1;
integer_signed_size(Number) when is_integer(Number) ->
	integer_signed_size(Number, 0).

integer_signed_size(Number, ByteCount) when Number < 0 ->
	integer_size(Number div -129, ByteCount + 1);
integer_signed_size(Number, ByteCount) when Number > 0 ->
	integer_size(Number div 128, ByteCount + 1).

integer_size(0) -> 1;
integer_size(Number) when is_integer(Number) ->
	integer_size(Number, 0).

integer_size(0, ByteCount) -> ByteCount;
integer_size(Number, ByteCount) ->
	integer_size(Number div 256, ByteCount + 1).

integer_to_binary(Number, Signedness, ByteOrder) when is_integer(Number) ->
	case {Signedness, ByteOrder} of
		{signed, big} ->
			IntegerSize = 8 * integer_signed_size(Number),
			<<Number:IntegerSize/signed-integer-big>>;
		{signed, little} ->
			IntegerSize = 8 * integer_signed_size(Number),
			<<Number:IntegerSize/signed-integer-little>>;
		{unsigned, big} ->
			IntegerSize = 8 * integer_size(Number),
			<<Number:IntegerSize/unsigned-integer-big>>;
		{unsigned, little} ->
			IntegerSize = 8 * integer_size(Number),
			<<Number:IntegerSize/unsigned-integer-little>>
	end.

setbit_count(Bitstring) when is_bitstring(Bitstring) ->
	setbit_count(Bitstring, 0).

setbit_count(<<>>, Count) -> Count;
setbit_count(<<1:1, Rest/bitstring>>, Count) ->
	setbit_count(Rest, Count + 1);
setbit_count(<<0:1, Rest/bitstring>>, Count) ->
	setbit_count(Rest, Count).

bit_parity(Bitstring) ->
	setbit_count(Bitstring) div 2 == 0.

head_unsetbit_count(Bitstring) when is_bitstring(Bitstring) ->
	head_unsetbit_count(Bitstring, 0).

head_unsetbit_count(<<>>, Count) -> Count;
head_unsetbit_count(<<0:1, Rest/bitstring>>, Count) ->
	head_unsetbit_count(Rest, Count + 1);
head_unsetbit_count(<<1:1, _Rest/bitstring>>, Count) -> Count.

tail_unsetbit_count(Bitstring) when is_bitstring(Bitstring) ->
	tail_unsetbit_count(Bitstring, 0).

tail_unsetbit_count(<<>>, Count) -> Count;
tail_unsetbit_count(Bitstring, Count) ->
	RestSize = bit_size(Bitstring) - 1,
	<<Rest:RestSize/bitstring, TailBit:1>> = Bitstring,
	case TailBit of
		0 -> tail_unsetbit_count(Rest, Count + 1);
		1 -> Count
	end.

inverse(Bitstring) when is_bitstring(Bitstring) ->
	<< <<(Bit bxor 1):1>> || <<Bit:1>> <= Bitstring >>.