#! /bin/bash
erl -noshell -pa ebin -eval "eunit:test([parallel_common, parallel_map, parallel_reduce, parallel_filter], [verbose])" -s init stop
