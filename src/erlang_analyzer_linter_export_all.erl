-module(erlang_analyzer_linter_export_all).

-behaviour(erlang_analyzer_linter).

-export([init/1]).

init(File) ->
  logger:notice("Initialising 'export_all' for ~p", [File]),
  {ok, []}.
