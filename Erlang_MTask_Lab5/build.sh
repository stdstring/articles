#! /bin/bash
erlc -Wall -Werror parallel_common.erl
erlc -Wall -Werror parallel_common_tests.erl
erlc -Wall -Werror parallel_portion_helper.erl
erlc -Wall -Werror parallel_limited_helper.erl
erlc -Wall -Werror parallel_smartmsg_helper.erl
erlc -Wall -Werror parallel_map.erl
erlc -Wall -Werror parallel_map_tests.erl
erlc -Wall -Werror parallel_reduce.erl
erlc -Wall -Werror parallel_reduce_tests.erl
erlc -Wall -Werror parallel_filter.erl
erlc -Wall -Werror parallel_filter_tests.erl
