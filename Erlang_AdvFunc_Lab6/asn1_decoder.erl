-module(asn1_decoder).

-include("asn1_tag.hrl").

-export([build/1, decode/2]).
-export([decode_tag/1, decode_length/1]).
-export([decode_boolean/2]).
-export([decode_integer/2]).
-export([decode_real/2]).
-export([decode_octetstring/2]).
-export([decode_bitstring/2]).
-export([decode_sequence/2]).
-export([decode_tuple/2]).
-export([decode_atom/2]).

build(ExternalDecoders) when is_list(ExternalDecoders) ->
	InternalDecoders =
	[
		{#tag{class = universal, form = primitive, tag_value = 1}, fun decode_boolean/2},
		{#tag{class = universal, form = primitive, tag_value = 2}, fun decode_integer/2},
		{#tag{class = universal, form = primitive, tag_value = 9}, fun decode_real/2},
		{#tag{class = universal, form = primitive, tag_value = 4}, fun decode_octetstring/2},
		{#tag{class = universal, form = primitive, tag_value = 3}, fun decode_bitstring/2},
		{#tag{class = universal, form = constructed, tag_value = 16}, fun decode_sequence/2},
		{#tag{class = universal, form = constructed, tag_value = 32}, fun decode_tuple/2},
		{#tag{class = universal, form = primitive, tag_value = 33}, fun decode_atom/2}
	],
	TotalDecodersList = ExternalDecoders ++ InternalDecoders,
	fun(Binary, DecodeDispatcher) ->
		{Tag, _BinaryRest} = decode_tag(Binary),
		case lists:keyfind(Tag, 1, TotalDecodersList) of
			{Tag, Decoder} -> {ok, Decoder(Binary, DecodeDispatcher)};
			false -> false
		end
	end.

decode(Value, DecodeDispatcher) ->
	case DecodeDispatcher(Value, DecodeDispatcher) of
		{ok, Result} -> Result;
		false -> erlang:error(unsuitable_value)
	end.

decode_tag(<<Class:2, Form:1, 2#11111:5, Rest/binary>>) ->
	{TagValue, TagRest} = decode_tag_value(Rest, []),
	{#tag{class = decode_tag_class(Class), form = decode_tag_form(Form), tag_value = TagValue}, TagRest};
decode_tag(<<Class:2, Form:1, TagValue:5, Rest/binary>>) ->
	{#tag{class = decode_tag_class(Class), form = decode_tag_form(Form), tag_value = TagValue}, Rest}.

decode_tag_class(2#00) -> universal;
decode_tag_class(2#01) -> application;
decode_tag_class(2#10) -> context_specific;
decode_tag_class(2#11) -> private.

decode_tag_form(0) -> primitive;
decode_tag_form(1) -> constructed.

decode_tag_value(<<0:1, Segment:7, Rest/binary>>, SegmentList) ->
	TagValueBitstring = list_to_bitstring(lists:reverse([<<Segment:7>>] ++ SegmentList)),
	BitSize = bit_size(TagValueBitstring),
	<<TagValue:BitSize/integer-big>>  = TagValueBitstring,
	{TagValue, Rest};
decode_tag_value(<<1:1, Segment:7, Rest/binary>>, SegmentList) ->
	decode_tag_value(Rest, [<<Segment:7>>] ++ SegmentList).

decode_length(<<0:1, Length:7, Rest/binary>>) -> {Length, Rest};
decode_length(<<1:1, LengthOctetCount:7, Rest/binary>>) ->
	LengthBitCount = 8 * LengthOctetCount,
	<<Length:LengthBitCount, ParseRest/binary>> = Rest,
	{Length, ParseRest}.

decode_boolean(<<1:8, 1:8, Value:8, Rest/binary>>, _DecodeDispatcher) ->
	if
		Value == 0 -> {false, Rest};
		Value /= 0 -> {true, Rest}
	end.

decode_integer(<<2:8, Rest/binary>>, _DecodeDispatcher) ->
	{OctetCount, OctetCountRest} = decode_length(Rest),
	Length = OctetCount * 8,
	<<Number:Length/integer-signed-big, ParseRest/binary>> = OctetCountRest,
	{Number, ParseRest}.

decode_real(<<9:8, 0:8, Rest/binary>>, _DecodeDispatcher) -> {0.0, Rest};
decode_real(<<9:8, 1:8, 2#01000000:8, Rest/binary>>, _DecodeDispatcher) -> {1.7976931348623157e308, Rest};
decode_real(<<9:8, 1:8, 2#01000001:8, Rest/binary>>, _DecodeDispatcher) -> {-1.7976931348623157e308, Rest};
decode_real(<<9:8, Rest/binary>>, _DecodeDispatcher) ->
	{TotalOctetCount, OctetCountRest} = decode_length(Rest),
	OctetCount = TotalOctetCount - 1,
	<<2#00:2, NR:6, RealBinary:OctetCount/binary, ParseRest/binary>> = OctetCountRest,
	RealStr = binary_to_list(RealBinary),
	case(decode_nr(NR)) of
		nr1 -> {list_to_integer(RealStr) * 1.0, ParseRest};
		nr2 -> {list_to_float(RealStr), ParseRest};
		nr3 -> {list_to_float(RealStr), ParseRest}
	end.

decode_nr(2#000001) -> nr1;
decode_nr(2#000010) -> nr2;
decode_nr(2#000011) -> nr3.

decode_octetstring(<<4:8, Rest/binary>>, _DecodeDispatcher) ->
	{OctetCount, OctetCountRest} = decode_length(Rest),
	<<Octet:OctetCount/binary, ParseRest/binary>> = OctetCountRest,
	{Octet, ParseRest}.

decode_bitstring(<<3:8, Rest/binary>>, _DecodeDispatcher) ->
	{OctetCount, OctetCountRest} = decode_length(Rest),
	<<UnusedBitCount:8, UnusedBitCountRest/binary>> = OctetCountRest,
	BitstringLength = 8 * (OctetCount - 1) - UnusedBitCount,
	<<Bitstring:BitstringLength/bitstring, _UnusedBits:UnusedBitCount, ParseRest/binary>> = UnusedBitCountRest,
	{Bitstring, ParseRest}.

decode_sequence(<<0:2, 1:1, 16:5, Rest/binary>>, DecodeDispatcher) ->
	{OctetCount, OctetCountRest} = decode_length(Rest),
	<<SequenceBinary:OctetCount/binary, SequenceRest/binary>> = OctetCountRest,
	Sequence = decode_sequence_content(SequenceBinary, DecodeDispatcher, []),
	{Sequence, SequenceRest}.

decode_tuple(<<0:2, 1:1, 2#11111:5, 32:8, Rest/binary>>, DecodeDispatcher) ->
	{OctetCount, OctetCountRest} = decode_length(Rest),
	<<SequenceBinary:OctetCount/binary, SequenceRest/binary>> = OctetCountRest,
	Sequence = decode_sequence_content(SequenceBinary, DecodeDispatcher, []),
	{list_to_tuple(Sequence), SequenceRest}.

decode_atom(<<0:2, 0:1, 2#11111:5, 33:8, Rest/binary>>, _DecodeDispatcher) ->
	{OctetCount, OctetCountRest} = decode_length(Rest),
	<<AtomBinary:OctetCount/binary, ParseRest/binary>> = OctetCountRest,
	{binary_to_atom(AtomBinary, utf8), ParseRest}.

decode_sequence_content(<<>>, _DecodeDispatcher, ContentList) ->
	lists:reverse(ContentList);
decode_sequence_content(Binary, DecodeDispatcher, ContentList) ->
	{DecodedElement, DecodeRest} = decode(Binary, DecodeDispatcher),
	decode_sequence_content(DecodeRest, DecodeDispatcher, [DecodedElement] ++ ContentList).