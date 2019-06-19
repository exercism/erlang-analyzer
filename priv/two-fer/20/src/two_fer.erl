-module(two_fer).

-export([two_fer/1, two_fer/0, test_version/0]).

two_fer() ->
  two_fer("you").

two_fer(Name) ->
  string:concat("One for ", string:concat(Name, ", one for me.")).

test_version() -> 1.