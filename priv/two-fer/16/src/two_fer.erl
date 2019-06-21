-module(two_fer).

-export([two_fer/1, two_fer/0, test_version/0]).

two_fer() -> two_fer("you").

two_fer(Name) ->
	unicode:characters_to_list(io_lib:format("One for ~s, one for me.", [Name])).


test_version() -> 1.