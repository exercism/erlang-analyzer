-module(common_rules_test).

-include_lib("eunit/include/eunit.hrl").

reports_no_false_positive_when_there_is_no_export_all_test() ->
  Config = #{project_path => filename:join([code:priv_dir(erlang_analyzer), "common-export-all", "1"])},
  File = filename:join("src", "export_selective.erl"),
  ?assertMatch([], ea_common_rules:no_export_all(Config, File, #{})).


positive_test_() ->
  Tests = [
    {          ["common-export-all",            "0"], ["export_all.erl"],   {no_export_all,        #{}}},
    {          ["common-no-test-version",       "0"], ["test_version.erl"], {no_test_version,      #{}}},
    {"simple", ["common-used-ignored-variable", "0"], ["use_ignored.erl"],  {use_ignored_variable, #{}}}
  ],
  positive_gen(Tests).

positive_gen([]) -> {generator, fun () -> [] end};
positive_gen([{P, F, {R, O}}|T]) -> positive_gen([{"", P, F, {R, O}}|T]);
positive_gen([{Name, Project, FileName, {Rule, Opts}}|T]) ->
  {generator, fun () ->
    Config    = #{project_path => filename:join([code:priv_dir(erlang_analyzer) | Project])},
    File      = filename:join(["src"|FileName]),
    TestName0 = atom_to_list(Rule) ++ " positive",
    TestName1 = case Name of
      "" -> TestName0;
      _  -> lists:flatten([TestName0, " <", Name, ">"])
    end,
    [{TestName1, ?_assertMatch(
      [#{rule := {ea_common_rules, Rule}}],
      ea_common_rules:Rule(Config, File, Opts)
    )}|positive_gen(T)]
  end}.

negative_test_() ->
  Tests = [
    {no_export_all,        #{}, [{["common-export-all",            "1"], ["export_selective.erl"]},
                                 {["common-no-test-version",       "0"], ["test_version.erl"]},
                                 {["common-used-ignored-variable", "0"], ["use_ignored.erl"]}]},
    {no_test_version,      #{}, [{["common-export-all",            "1"], ["export_selective.erl"]},
                                 {["common-export-all",            "0"], ["export_all.erl"]},
                                 {["common-used-ignored-variable", "0"], ["use_ignored.erl"]}]},
    {use_ignored_variable, #{}, [{["common-export-all",            "1"], ["export_selective.erl"]},
                                 {["common-export-all",            "0"], ["export_all.erl"]},
                                 {["common-no-test-version",       "0"], ["test_version.erl"]}]}
  ],
  negative_gen(Tests).

negative_gen([]) -> {generator, fun () -> [] end};
negative_gen([{_Rule, _Opts, []} | Tail]) -> negative_gen(Tail);
negative_gen([{Rule, Opts, [{Project, FileName}|Other]}|Tail]) ->
  {generator, fun () ->
    Config = #{project_path => filename:join([code:priv_dir(erlang_analyzer) | Project])},
    File   = filename:join(["src"|FileName]),
    [?_assertMatch([], ea_common_rules:Rule(Config, File, Opts))|negative_gen([{Rule, Opts, Other}|Tail])]
  end}.