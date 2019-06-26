-module(two_fer_test).

-include_lib("eunit/include/eunit.hrl").

positive_test_() ->
  Tests = [
    {          ["two-fer", "0"], ["two_fer.erl"], {flatten_flat_list, #{}}}
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
      [#{rule := {ea_two_fer_rules, Rule}}],
      ea_two_fer_rules:Rule(Config, File, Opts)
    )}|positive_gen(T)]
  end}.