-module(common_rules_test).

-include_lib("eunit/include/eunit.hrl").

positive_test_() ->
  Tests = [
    {["common-export-all",            "0"], ["export_all.erl"],   {no_export_all,        #{}}},
    {["common-no-test-version",       "0"], ["test_version.erl"], {no_test_version,      #{}}},
    {["common-used-ignored-variable", "0"], ["use_ignored.erl"],  {use_ignored_variable, #{}}}
  ],
  positive_gen(Tests).

positive_gen([]) -> {generator, fun () -> [] end};
positive_gen([{P, F, {R, O}}|T]) -> positive_gen([{"", P, F, {R, O}}|T]);
positive_gen([{Name, Project, FileName, {Rule, Opts}}|T]) ->
  {generator, fun () ->
    Config    = #{project_path => filename:join([code:priv_dir(erlang_analyzer) | Project])},
    File      = filename:join(["src"|FileName]),
    [{lists:flatten(io_lib:format("~s does trigger ~p", [filename:join(Project ++ ["src"|FileName]), Rule])),
    ?_assertMatch(
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
    [{lists:flatten(io_lib:format("~s does not trigger ~p", [filename:join(Project ++ ["src"|FileName]), Rule])),
      ?_assertMatch([], ea_common_rules:Rule(Config, File, Opts))}|negative_gen([{Rule, Opts, Other}|Tail])]
  end}.