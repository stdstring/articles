-module(datetime_formatter).

-include("datetime_formatter.hrl").

-export([format/2, smart_format/3]).

%% spec. char sequences DD - day, MM - month, YY or YYYY - year, hh - hour, mm - minutes, ss - seconds, ms - milliseconds
format(FormatString, DateTime) -> format(FormatString, DateTime, "").

format([], _DateTime, Dest) -> lists:reverse(Dest);
format([$D, $D | Rest], DateTime, Dest) -> format(Rest, DateTime, integer_to_rstring(DateTime#datetime.day, 2) ++ Dest);
format([$M, $M | Rest], DateTime, Dest) -> format(Rest, DateTime, integer_to_rstring(DateTime#datetime.month, 2) ++ Dest);
format([$Y, $Y, $Y, $Y | Rest], DateTime, Dest) -> format(Rest, DateTime, integer_to_rstring(DateTime#datetime.year, 4) ++ Dest);
format([$Y, $Y | Rest], DateTime, Dest) -> format(Rest, DateTime, integer_to_rstring(DateTime#datetime.year rem 100, 2) ++ Dest);
format([$h, $h | Rest], DateTime, Dest) -> format(Rest, DateTime, integer_to_rstring(DateTime#datetime.hour, 2) ++ Dest);
format([$m, $m | Rest], DateTime, Dest) -> format(Rest, DateTime, integer_to_rstring(DateTime#datetime.minutes, 2) ++ Dest);
format([$s, $s | Rest], DateTime, Dest) -> format(Rest, DateTime, integer_to_rstring(DateTime#datetime.seconds, 2) ++ Dest);
format([$m, $s | Rest], DateTime, Dest) -> format(Rest, DateTime, integer_to_rstring(DateTime#datetime.milliseconds, 3) ++ Dest);
format([Char | Rest], DateTime, Dest) when is_integer(Char), Char > 0, Char < 256 -> format(Rest, DateTime, [Char] ++ Dest).

smart_format(FormatString, FormatConfig, DateTime) ->
	Handlers = [fun(Input, DT) -> format_part(Input, FormatConfig#format_config.day, DT#datetime.day) end,
				fun(Input, DT) -> format_part(Input, FormatConfig#format_config.month, DT#datetime.month) end,
				fun(Input, DT) -> format_year(Input, FormatConfig#format_config.year, DT#datetime.year) end,
				fun(Input, DT) -> format_syear(Input, FormatConfig#format_config.short_year, DT#datetime.year) end,
				fun(Input, DT) -> format_part(Input, FormatConfig#format_config.hour, DT#datetime.hour) end,
				fun(Input, DT) -> format_part(Input, FormatConfig#format_config.minutes, DT#datetime.minutes) end,
				fun(Input, DT) -> format_part(Input, FormatConfig#format_config.seconds, DT#datetime.seconds) end,
				fun(Input, DT) -> format_ms(Input, FormatConfig#format_config.milliseconds, DT#datetime.milliseconds) end,
				fun(Input, _DT) -> format_other_char(Input) end],
	process_format(FormatString, DateTime, Handlers, []).

process_format([], _DateTime, _Handlers, Dest) -> lists:reverse(Dest);
process_format(FormatString, DateTime, Handlers, Dest) ->
	{Data, FormatStringRest} = iterate_handlers(FormatString, DateTime, Handlers),
	process_format(FormatStringRest, DateTime, Handlers, Data ++ Dest).
	
iterate_handlers(_Input, _DateTime, []) -> erlang:error(bad_formatstring);
iterate_handlers(Input, DateTime, [Handler | OtherHandlers]) ->
	case Handler(Input, DateTime) of
		{true, Data, InputRest} -> {Data, InputRest};
		false -> iterate_handlers(Input, DateTime, OtherHandlers)
	end.

format_part(FormatString, FormatPart, DTPart) ->
	case lists:prefix(FormatPart, FormatString) of
		true -> {true, integer_to_rstring(DTPart, 2), string:sub_string(FormatString, length(FormatPart)+1)};
		false -> false
	end.
	
format_year(FormatString, FormatYearPart, Year) ->
	case lists:prefix(FormatYearPart, FormatString) of
		true -> {true, integer_to_rstring(Year, 4), string:sub_string(FormatString, length(FormatYearPart)+1)};
		false -> false
	end.
	
format_syear(FormatString, FormatShortYearPart, Year) ->
	case lists:prefix(FormatShortYearPart, FormatString) of
		true -> {true, integer_to_rstring(Year rem 100, 2), string:sub_string(FormatString, length(FormatShortYearPart)+1)};
		false -> false
	end.
	
format_ms(FormatString, FormatMillisecondsPart, Milliseconds) ->
	case lists:prefix(FormatMillisecondsPart, FormatString) of
		true -> {true, integer_to_rstring(Milliseconds, 3), string:sub_string(FormatString, length(FormatMillisecondsPart)+1)};
		false -> false
	end.
	
format_other_char([Char | Rest]) when is_integer(Char), Char > 0, Char < 256 ->	{true, [Char], Rest}.

integer_to_rstring(Number, ExpectedLength) ->
	StringRepr = integer_to_list(Number),
	lists:reverse(StringRepr) ++ string:chars($0, ExpectedLength-length(StringRepr)).