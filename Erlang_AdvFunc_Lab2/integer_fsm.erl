-module(integer_fsm).

-include("state_data.hrl").

-export([start/2, sign/2, digit/2]).

start({char, Char}, {start, _StateData}) when Char == $+; Char == $- ->
	{sign, #state_data{pull = [Char]}};
start({char, Char}, {start, _StateData}) when Char >= $0, Char =< $9 ->
	{digit, #state_data{pull = [Char], recognized = [Char], is_complete = true}};
start({char, _Char}, {start, _StateData}) ->
	{unrecognized, #state_data{}}.
	
sign({char, Char}, {sign, StateData}) when Char >= $0, Char =< $9 ->
	{digit, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.pull ++ [Char], is_complete = true}};
sign({char, _Char}, {sign, StateData}) ->
	{unrecognized, StateData}.
	
digit({char, Char}, {digit, StateData}) when Char >= $0, Char =< $9 ->
	{digit, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.pull ++ [Char], is_complete = true}};
digit({char, _Char}, {digit, StateData}) ->
	{unrecognized, StateData}.