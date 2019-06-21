-module(common_rules_test).

-include_lib("eunit/include/eunit.hrl").

reports_when_export_all_is_used_test() ->
  Config = #{project_path => filename:join([code:priv_dir(erlang_analyzer), "common-export-all", "0"])},
  File = filename:join("src", "export_all.erl"),
  ?assertMatch([#{rule := {erlang_analyzer_common_rules,no_export_all}}], erlang_analyzer_common_rules:no_export_all(Config, File, #{})).

reports_no_false_positive_when_there_is_no_export_all_test() ->
  Config = #{project_path => filename:join([code:priv_dir(erlang_analyzer), "common-export-all", "1"])},
  File = filename:join("src", "export_selective.erl"),
  ?assertMatch([], erlang_analyzer_common_rules:no_export_all(Config, File, #{})).
