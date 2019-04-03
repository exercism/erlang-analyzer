-module(export_all_test).

-include_lib("eunit/include/eunit.hrl").

does_not_allow_export_all_test() ->
  Code = "-module(export_all_example).\n"
    "-compile(export_all).\n"
    "foo() -> undefined.\n",
  {ok, #{export_all := ExportAll}} = erlang_analyzer:analyze("example/export_all_example.erl", Code),
  ?assertNotEqual([], ExportAll).
