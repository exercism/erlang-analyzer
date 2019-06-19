-module(erlang_analyzer_app).

-behaviour(application).
-export([start/2, stop/1]).

<<<<<<< HEAD
start(normal, []) ->
  logger:notice("Starting application"),
  erlang_analyzer_sup:start_link([]).
=======
-define(ALL_LINTERS, [
  export_all,
  two_fer
]).

start(normal, []) ->
  logger:notice("Starting application"),
  erlang_analyzer_sup:start_link(?ALL_LINTERS).
>>>>>>> master

stop(_) -> ok.