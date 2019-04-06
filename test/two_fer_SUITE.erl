-module(two_fer_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test Callbacks
-export([all/0, init_per_testcase/2]).

%% Testcases
-export([two_fer_0/1]).

all() ->
  [two_fer_0].

init_per_testcase(TestCase, Config) when is_atom(TestCase)->
  init_per_testcase(atom_to_list(TestCase), Config);
init_per_testcase("two_fer_" ++ ID, Config) ->
  IntID = list_to_integer(ID),
  Path  = filename:join(?config(data_dir, Config), ID),
  {ok, Sup} = erlang_analyzer_sup:start_link([two_fer]),
  ok = erlang_analyzer_linter:prepare(two_fer, "two_fer.erl"),
  {ok, Result} = analyze(Path, "two_fer.erl"),
  [{result, Result}, {id, IntID}, {path, Path}, {supervisor, Sup}|Config].


two_fer_0(Config) ->
  Dir = filename:join(?config(data_dir, Config), "0"),
  ct:print(Dir).

%% Auxiliary functions

analyze(Path, Name) ->
  FullPath = filename:join(Path, Name),
  {ok, FD} = file:open(FullPath, [read]),
  {ok, Forms} = epp_dodger:parse(FD),
  lists:map(
    fun (Form) -> erlang_analyzer_linter:check(two_fer, Name, Form) end,
    Forms),
  Result = erlang_analyzer_linter:get(two_fer, Name),
  {ok, Result}.
