-module(erlang_analyzer_app).

-behaviour(application).
-export([start/2, stop/1]).

-define(ALL_LINTERS, [
  export_all,
  two_fer
]).

start(normal, []) ->
  erlang_analyzer_sup:start_link(?ALL_LINTERS).

stop(_) -> ok.