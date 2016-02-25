-module(real_fsm).

-include("state_data.hrl").

-export([start/2, int_part_sign/2, int_part_digit/2, fract_part_separator/2, fract_part_digit/2, exp_part_separator/2, exp_part_sign/2, exp_part_digit/2]).

start({char, Char}, {start, _StateData}) when Char == $+; Char == $- ->
	{int_part_sign, #state_data{pull = [Char]}};
start({char, Char}, {start, _StateData}) when Char >= $0, Char =< $9 ->
	{int_part_digit, #state_data{pull = [Char]}};
start({char, _Char}, {start, _StateData}) ->
	{unrecognized, #state_data{}}.

int_part_sign({char, Char}, {int_part_sign, StateData}) when Char >= $0, Char =< $9 ->
	{int_part_digit, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.pull ++ [Char], is_complete = true}};
int_part_sign({char, _Char}, {int_part_sign, StateData}) ->
	{unrecognized, StateData}.

int_part_digit({char, Char}, {int_part_digit, StateData}) when Char >= $0, Char =< $9 ->
	{int_part_digit, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.pull ++ [Char], is_complete = true}};
int_part_digit({char, Char}, {int_part_digit, StateData}) when Char == $. ->
	{fract_part_separator, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.recognized}};
int_part_digit({char, Char}, {int_part_digit, StateData}) when Char == $E; Char == $e ->
	{exp_part_separator, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.recognized}};
int_part_digit({char, _Char}, {int_part_digit, StateData}) ->
	{unrecognized, StateData}.

fract_part_separator({char, Char}, {fract_part_separator, StateData}) when Char >= $0, Char =< $9 ->
	{fract_part_digit, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.pull ++ [Char], is_complete = true}};
fract_part_separator({char, _Char}, {fract_part_separator, StateData}) ->
	{unrecognized, StateData}.

fract_part_digit({char, Char}, {fract_part_digit, StateData}) when Char >= $0, Char =< $9 ->
	{fract_part_digit, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.pull ++ [Char], is_complete = true}};
fract_part_digit({char, Char}, {fract_part_digit, StateData}) when Char == $E; Char == $e ->
	{exp_part_separator, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.recognized}};
fract_part_digit({char, _Char}, {fract_part_digit, StateData}) ->
	{unrecognized, StateData}.

exp_part_separator({char, Char}, {exp_part_separator, StateData}) when Char == $+; Char == $- ->
	{exp_part_sign, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.recognized}};
exp_part_separator({char, Char}, {exp_part_separator, StateData}) when Char >= $0, Char =< $9 ->
	{exp_part_digit, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.pull ++ [Char], is_complete = true}};
exp_part_separator({char, _Char}, {exp_part_separator, StateData}) ->
	{unrecognized, StateData}.

exp_part_sign({char, Char}, {exp_part_sign, StateData}) when Char >= $0, Char =< $9 ->
	{exp_part_digit, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.pull ++ [Char], is_complete = true}};
exp_part_sign({char, _Char}, {exp_part_sign, StateData}) ->
	{unrecognized, StateData}.

exp_part_digit({char, Char}, {exp_part_digit, StateData}) when Char >= $0, Char =< $9 ->
	{exp_part_digit, #state_data{pull = StateData#state_data.pull ++ [Char], recognized = StateData#state_data.pull ++ [Char], is_complete = true}};
exp_part_digit({char, _Char}, {exp_part_digit, StateData}) ->
	{unrecognized, StateData}.