-record(mock_repository, {name = "default_mock_repository", expectations = []}).
-record(mock, {name = "", arity = 0}).
-record(expectation, {mock_name = "", parameters = []}).