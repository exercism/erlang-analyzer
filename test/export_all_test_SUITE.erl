-module(export_all_test_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
  [does_not_allow_export_all,
   does_allow_regular].

init_per_testcase(does_not_allow_export_all, Config) -> [{file_name, "export_all_example_bad.erl"} |Config];
init_per_testcase(does_allow_regular,        Config) -> [{file_name, "export_all_example_good.erl"}|Config].

does_not_allow_export_all(Config) ->
  {data_dir,  Dir}  = lists:keyfind(data_dir,  1, Config),
  {file_name, File} = lists:keyfind(file_name, 1, Config),
  case erlang_analyzer:analyze(Dir, File) of
    {ok, #{export_all := [{export_all, 2}]}} -> ok;
    _ -> {skip, unimplemented}
  end.

does_allow_regular(Config) ->
  {data_dir,  Dir}  = lists:keyfind(data_dir,  1, Config),
  {file_name, File} = lists:keyfind(file_name, 1, Config),
  case erlang_analyzer:analyze(Dir, File) of
    {ok, #{export_all := []}} -> ok;
    _ -> {skip, unimplemented}
  end.
