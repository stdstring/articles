-module(asn1_encoder).

-include("asn1_tag.hrl").

-export([build/1, encode/2]).
-export([encode_tag/1, encode_length/1]).
-export([encode_boolean/2]).
-export([encode_integer/2]).
-export([encode_real/2]).
-export([encode_octetstring/2]).
-export([encode_bitstring/2]).
-export([encode_sequence/2]).
-export([encode_tuple/2]).
-export([encode_atom/2]).

build(ExternalEncoders) when is_list(ExternalEncoders) ->
	InternalEncoders = 
	[
		{fun is_boolean/1, fun encode_boolean/2},
		{fun is_integer/1, fun encode_integer/2},
		{fun is_float/1, fun encode_real/2},
		{fun is_binary/1, fun encode_octetstring/2},
		{fun is_bitstring/1, fun encode_bitstring/2},
		{fun is_list/1, fun encode_sequence/2},
		{fun is_tuple/1, fun encode_tuple/2},
		{fun is_atom/1, fun encode_atom/2}
	],
	EncodersList = ExternalEncoders ++ InternalEncoders,
	fun(Value, Dispatcher) -> first(EncodersList, Value, Dispatcher) end.

encode(Value, EncodeDispatcher) ->
	case EncodeDispatcher(Value, EncodeDispatcher) of
		{ok, Result} -> Result;
		false -> erlang:error(unsuitable_value)
	end.

encode_tag(#tag{class = Class, form = Form, tag_value = Value}) ->
	list_to_bitstring([encode_tag_class(Class), encode_tag_form(Form), encode_tag_value(Value)]).

encode_tag_class(universal) -> <<2#00:2>>;
encode_tag_class(application) -> <<2#01:2>>;
encode_tag_class(context_specific) -> <<2#10:2>>;
encode_tag_class(private) -> <<2#11:2>>.

encode_tag_form(primitive) -> <<0:1>>;
encode_tag_form(constructed) -> <<1:1>>.

encode_tag_value(Value) when (Value >= 0) and (Value =< 30) -> <<Value:5>>;
encode_tag_value(Value) when Value >= 31 ->
	%% 2^7 = 128
	SegmentCount = (Value div 128) + 1,
	SegmentList = encode_tag_value(<<Value:(SegmentCount * 7)>>, []),
	list_to_bitstring([<<2#11111:5>>] ++ lists:reverse(SegmentList)).

encode_tag_value(<<Segment:7>>, SegmentList) -> [<<0:1, Segment:7>>] ++ SegmentList;
encode_tag_value(<<Segment:7, Rest/bitstring>>, SegmentList) ->
	encode_tag_value(Rest, [<<1:1, Segment:7>>] ++ SegmentList).

encode_length(LengthValue) when (LengthValue >= 0) and (LengthValue =< 127) -> <<0:1, LengthValue:7>>;
encode_length(LengthValue) when LengthValue >= 128 ->
	OctetCount = (LengthValue div 256) + 1,
	list_to_binary([<<1:1, OctetCount:7>>] ++ [binary:encode_unsigned(LengthValue, big)]).

encode_boolean(true, _EncodeDispatcher) ->
	Tag = encode_tag(#tag{class = universal, form = primitive, tag_value = 1}),
	list_to_binary([Tag, encode_length(1), <<2#11111111:8>>]);
encode_boolean(false, _EncodeDispatcher) ->
	Tag = encode_tag(#tag{class = universal, form = primitive, tag_value = 1}),
	list_to_binary([Tag, encode_length(1), <<2#00000000:8>>]).

encode_integer(Number, _EncodeDispatcher) ->
	Tag = encode_tag(#tag{class = universal, form = primitive, tag_value = 2}),
	NumberBinary = encode_integer_value(Number),
	list_to_binary([Tag, encode_length(size(NumberBinary)), NumberBinary]).

encode_integer_value(Number) when Number >= 0 ->
	OctetCount = get_octet_count(Number, 0),
	NumberBinary = <<Number:(8 * OctetCount)/integer-signed-big>>,
	<<OldestBit:1, _Rest/bitstring>> = NumberBinary,
	if
		OldestBit == 1 -> list_to_binary([<<0:8>>, NumberBinary]);
		OldestBit == 0 -> NumberBinary
	end;
encode_integer_value(Number) when Number < 0 ->
	OctetCount = get_octet_count(Number, 0),
	<<Number:(8 * OctetCount)/integer-signed-big>>.

get_octet_count(0, 0) -> 1;
get_octet_count(0, Count) -> Count;
get_octet_count(Number, 0) when Number < 0 -> get_octet_count(Number div -129, 1);
get_octet_count(Number, Count) -> get_octet_count(Number div 256, Count + 1).

encode_real(0.0, _EncodeDispatcher) ->
	Tag = encode_tag(#tag{class = universal, form = primitive, tag_value = 9}),
	list_to_binary([Tag, <<0:8>>]);
encode_real(Number, _EncodeDispatcher) ->
	Tag = encode_tag(#tag{class = universal, form = primitive, tag_value = 9}),
	NumberStr = float_to_list(Number),
	%% allways use NR3
	list_to_binary([Tag, encode_length(length(NumberStr) + 1), <<2#00000011>>, NumberStr]).

encode_octetstring(OctetString, _EncodeDispatcher) ->
	Tag = encode_tag(#tag{class = universal, form = primitive, tag_value = 4}),
	list_to_binary([Tag, encode_length(size(OctetString)), OctetString]).

encode_bitstring(BitString, _EncodeDispatcher) ->
	OctetCount = (bit_size(BitString) div 8) + 1,
	UnusedBitCount = 8 - bit_size(BitString) rem 8,
	Tag = encode_tag(#tag{class = universal, form = primitive, tag_value = 3}),
	EncodedValue = list_to_bitstring([BitString, <<0:UnusedBitCount>>]),
	list_to_binary([Tag, encode_length(OctetCount + 1), <<UnusedBitCount:8>>, EncodedValue]).

encode_sequence(Sequence, EncodeDispatcher) ->
	Tag = encode_tag(#tag{class = universal, form = constructed, tag_value = 16}),
	{ContentLength, ContentBinary} = encode_sequence_content(Sequence, EncodeDispatcher),
	list_to_binary([Tag, encode_length(ContentLength), ContentBinary]).

%% defined by us
encode_tuple(Tuple, EncodeDispatcher) ->
	Tag = encode_tag(#tag{class = universal, form = constructed, tag_value = 32}),
	{ContentLength, ContentBinary} = encode_sequence_content(tuple_to_list(Tuple), EncodeDispatcher),
	list_to_binary([Tag, encode_length(ContentLength), ContentBinary]).

encode_atom(Atom, _EncodeDispatcher) ->
	Tag = encode_tag(#tag{class = universal, form = primitive, tag_value = 33}),
	AtomBinary = atom_to_binary(Atom, utf8),
	list_to_binary([Tag, encode_length(size(AtomBinary)), AtomBinary]).

encode_sequence_content(Sequence, EncodeDispatcher) ->
	lists:foldl(fun(Element, {Length, Binary}) ->
		EncodedElement = encode(Element, EncodeDispatcher),
		EncodedSize = size(EncodedElement),
		{Length + EncodedSize, list_to_binary([Binary, EncodedElement])}
	end, {0, <<>>}, Sequence).

%% specific first
first([], _Value, _EncoderDispatcher) -> false;
first([{Predicate, Encoder} | Rest], Value, EncoderDispatcher) ->
	case Predicate(Value) of
		true -> {ok, Encoder(Value, EncoderDispatcher)};
		false -> first(Rest, Value, EncoderDispatcher)
	end.