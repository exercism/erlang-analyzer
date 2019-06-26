-module(two_fer_test).

-include_lib("eunit/include/eunit.hrl").

-define(RULE_LIST, [
  {"0", [flatten_flat_list], []}
]).

% positive_test_() ->
%   Tests = [
%     {          ["two-fer", "0"], ["two_fer.erl"], {flatten_flat_list, #{}}}
%   ],
%   positive_gen(Tests).

% positive_gen([]) -> {generator, fun () -> [] end};
% positive_gen([{P, F, {R, O}}|T]) -> positive_gen([{"", P, F, {R, O}}|T]);
% positive_gen([{Name, Project, FileName, {Rule, Opts}}|T]) ->
%   {generator, fun () ->
%     Config    = #{project_path => filename:join([code:priv_dir(erlang_analyzer) | Project])},
%     File      = filename:join(["src"|FileName]),
%     TestName0 = atom_to_list(Rule) ++ " positive",
%     TestName1 = case Name of
%       "" -> TestName0;
%       _  -> lists:flatten([TestName0, " <", Name, ">"])
%     end,
%     [{TestName1, ?_assertMatch(
%       [#{rule := {ea_two_fer_rules, Rule}}],
%       ea_two_fer_rules:Rule(Config, File, Opts)
%     )}|positive_gen(T)]
%   end}.

two_fer_test_() ->
  PositiveTests = lists:flatmap(fun ({Proj, Positives, _Negatives}) ->
    lists:map(fun (Rule) -> {Proj, Rule} end, Positives)
  end, ?RULE_LIST),
  NegativeTests = lists:flatmap(fun ({Proj, _Positives, Negatives}) ->
    lists:map(fun (Rule) -> {Proj, Rule} end, Negatives)
  end, ?RULE_LIST),
  positive_gen(PositiveTests, NegativeTests).

positive_gen([], NegativeTests) -> negative_gen(NegativeTests);
positive_gen([{Proj, Rule}|Tail], NegativeTests) ->
  Config   = #{project_path => filename:join([code:priv_dir(erlang_analyzer), "two-fer", Proj])},
  File     = filename:join(["src", "two_fer.erl"]),
  TestName = "two-fer " ++ atom_to_list(Rule) ++ " finds something in " ++ filename:join(["two-fer", Proj, "src", "two_fer.erl"]),
  {generator, fun () ->
    [{TestName, ?_assertMatch(
      [#{rule := {ea_two_fer_rules, Rule}}],
      ea_two_fer_rules:Rule(Config, File, #{})
    )}|positive_gen(Tail, NegativeTests)]
  end}.

negative_gen([]) -> [].