-module(erlang_analyzer).

%% API exports
-export([main/1, start/0, start/1, analyze/2]).

-define(ALL_LINTERS,
        [export_all]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([_, File]) ->
  try start() of
      _ -> ok
  catch
    Type:Error:Trace ->
      io:format("An error occured during start: ~p:~p~n~p~n", [Type, Error, Trace]),
      erlang:halt(1)
  end,
  io:format("Application booted~n"),
  try init(File) of
      _ -> ok
  catch
    Type1:Error1:Trace1 ->
      io:format("An error occured during start: ~p:~p~n~p~n", [Type1, Error1, Trace1]),
      erlang:halt(2)
  end,

  erlang:halt(0).

start() -> start(?ALL_LINTERS).

start(Linters) when is_list(Linters) ->
  io:format("supervisor result: ~p~n", [erlang_analyzer_sup:start_link(Linters)]).

init(File) -> init(File, ?ALL_LINTERS).

init(_File, Linters) when is_list(Linters) ->
  ok.

analyze(_Name, _Code) ->
  {ok, #{export_all => []}}.

%%====================================================================
%% Internal functions
%%====================================================================
