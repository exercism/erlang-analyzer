-module(two_fer).

-export([two_fer/1, two_fer/0, test_version/0]).

-spec two_fer() -> string().
two_fer() ->
  two_fer("you").

-spec two_fer(string()) -> string().
two_fer(Name) ->
  lists:flatten(io_lib:format("One for ~s, one for me.", [Name])).

test_version() -> 1.
