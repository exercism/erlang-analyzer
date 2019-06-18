-module(erlang_analyzer_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Linters) when is_list(Linters)->
  supervisor:start_link(?MODULE, Linters).

init(Linters) ->
  logger:notice("Starting supervisor"),
  SupervisorFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  ChildSpecs = lists:map(fun (Linter) ->
                             #{id      => Linter,
                               start   => {erlang_analyzer_linter,
                                           start_link,
                                           [Linter]},
                               type    => worker,
                               modules => [Linter]}
                         end, Linters),
  {ok, {SupervisorFlags, ChildSpecs}}.
