-module(mock_framework).

-include("mock_defs.hrl").

-export([create_mock_repository/0, create_mock_repository/1, create_mock/2, create_expectation/3]).

create_mock_repository() -> #mock_repository{}.

create_mock_repository(RepositoryName) -> #mock_repository{name = RepositoryName}.

create_mock(Name, Arity) -> #mock{name = Name, arity = Arity}.

create_expectation(MockRepository, MockName, Parameters) ->
    Expectation = #expectation{mock_name = MockName, parameters = Parameters},
    Old = MockRepository#mock_repository.expectations,
    MockRepository#mock_repository{expectations = Old ++ [Expectation]}.