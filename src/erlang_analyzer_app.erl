-module(erlang_analyzer_app).

-behaviour(application).
-export([start/2, stop/1]).

start(normal, []) ->
  logger:notice("Starting application"),
  erlang_analyzer_sup:start_link([]).

stop(_) -> ok.