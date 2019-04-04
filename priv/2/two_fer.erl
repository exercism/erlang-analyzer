-module(two_fer).

-export([test_version/0, two_fer/0, two_fer/1]).

two_fer() -> two_fer("you").

two_fer(Name) ->
    string:join(["One for ", Name, ", one for me."], "").

test_version() -> 1.
