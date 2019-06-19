-module(two_fer_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test Callbacks
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_suite/1]).

%% Testcases
-export([two_fer_0/1]).

all() ->
  [two_fer_0].

%% We need to alter the data_dir to point into `priv/two-fer` since rebar would compile the examples otherwise
init_per_suite(Config) ->
  SampleBaseDir = filename:join(code:priv_dir(erlang_analyzer), "two-fer"),
  [{sample_base_dir, SampleBaseDir} | Config].

end_per_suite(Config) -> Config.

init_per_testcase(TestCase, Config) when is_atom(TestCase)->
  init_per_testcase(atom_to_list(TestCase), Config);
init_per_testcase("two_fer_" ++ ID, Config) ->
  IntID = list_to_integer(ID),
  Path  = filename:join(?config(sample_base_dir, Config), ID),
  {ok, Sup} = erlang_analyzer_sup:start_link([two_fer]),
  ok = erlang_analyzer_linter:prepare(two_fer, "two_fer.erl"),
  % {ok, Result} = analyze(Path, "two_fer.erl"),
  Result = analyze(Path, "two_fer.erl"),
  ct:print("~p", [Result]),
  [{result, Result}, {id, IntID}, {sample_dir, Path}, {supervisor, Sup}|Config].


two_fer_0(Config) ->
  Dir = filename:join([?config(sample_dir, Config), "0", "two-fer"]),
  ct:print("~p", [Config]).

%% Auxiliary functions

analyze(Path, Name) ->
  FullPath = filename:join([Path, "src", Name]),
  {ok, FileContents} = file:read_file(FullPath),
  {ok, Forms} = ktn_code:parse_tree(FileContents),
  lists:map(
    fun (Form) -> erlang_analyzer_linter:check(two_fer, Name, Form) end,
    Forms),
  Result = erlang_analyzer_linter:get(two_fer, Name),
  {ok, Result}.

dbg(Term) ->
  ct:print("~p~n", [Term]), Term.
