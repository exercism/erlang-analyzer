-module(two_fer).

-export([two_fer/1, two_fer/0, test_version/0]).

%% API

-spec two_fer() -> nonempty_string().
two_fer() ->
    two_fer("you").

-spec two_fer(string()) -> nonempty_string().
two_fer(Name) ->
    "One for " ++ Name ++ ", one for me.".

-spec test_version() -> integer().
test_version() ->
    1.
