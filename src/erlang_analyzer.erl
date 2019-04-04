-module(erlang_analyzer).

%% API exports
-export([main/1, start/0, start/1, analyze/2, analyze/3]).

-define(ALL_LINTERS,
        [export_all]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Exercise, BasePath]) ->
  try start() of
      _ -> ok
  catch
    Type:Error:Trace ->
      io:format("An error occured during start: ~p:~p~n~p~n", [Type, Error, Trace]),
      erlang:halt(1)
  end,
  io:format("Application booted~n"),
  try init(BasePath) of
      _ -> ok
  catch
    Type1:Error1:Trace1 ->
      io:format("An error occured during start: ~p:~p~n~p~n", [Type1, Error1, Trace1]),
      erlang:halt(2)
  end,
  timer:sleep(5000),
  erlang:halt(0).

start() -> start(?ALL_LINTERS).

start(Linters) when is_list(Linters) ->
  io:format("supervisor result: ~p~n", [erlang_analyzer_sup:start_link(Linters)]).

init(BasePath) -> init(BasePath, ?ALL_LINTERS).

init(BasePath, Linters) when is_list(Linters) ->
  Files = filelib:wildcard("**/*.erl", BasePath),
  io:format("Files: ~p~n", [Files]),
  FileLinter =
    lists:flatmap(fun (File) ->
                      lists:map(fun (Linter) -> {File, Linter} end, Linters)
                  end, Files),
  io:format("FileLinters: ~p~n", [FileLinter]),
  lists:map(fun ({File, Linter}) -> erlang_analyzer_linter:prepare(Linter, File) end,
            FileLinter).

analyze(BasePath, File) ->
  FullPath = filename:join(BasePath, File),
  Code     = file:read_file(FullPath),
  analyze(BasePath, File, Code).

analyze(BasePath, File, Code) when is_list(Code) ->
  analyze(BasePath, File, unicode:characters_to_binary(Code));
analyze(BasePath, File, Code) ->
  analyze(BasePath, File, Code, ?ALL_LINTERS).

analyze(BasePath, File, Code, Linters) when is_list(Linters) ->
  {ok, #{export_all => []}}.

%%====================================================================
%% Internal functions
%%====================================================================
