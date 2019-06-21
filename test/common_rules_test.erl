-module(common_rules_test).

-include_lib("eunit/include/eunit.hrl").

reports_when_export_all_is_used_test() ->
  Config = #{project_path => filename:join([code:priv_dir(erlang_analyzer), "common-export-all", "0"])},
  File = filename:join("src", "export_all.erl"),
  ?assertMatch([#{rule := {ea_common_rules,no_export_all}}], ea_common_rules:no_export_all(Config, File, #{})).

reports_no_false_positive_when_there_is_no_export_all_test() ->
  Config = #{project_path => filename:join([code:priv_dir(erlang_analyzer), "common-export-all", "1"])},
  File = filename:join("src", "export_selective.erl"),
  ?assertMatch([], ea_common_rules:no_export_all(Config, File, #{})).

reports_when_there_is_a_test_version_exported_test() ->
  Config = #{project_path => filename:join([code:priv_dir(erlang_analyzer), "common-no-test-version", "0"])},
  File = filename:join("src", "test_version.erl"),
  ?assertMatch([#{rule := {ea_common_rules, no_test_version}}], ea_common_rules:no_test_version(Config, File, #{})).
