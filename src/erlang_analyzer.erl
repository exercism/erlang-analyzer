-module(erlang_analyzer).

%% API exports
-export([main/1, analyze/2, analyze/3]).

-define(ALL_LINTERS,
        [export_all]).

%%====================================================================
%% API functions
%%====================================================================

main([Exercise, BasePath]) ->
  ok = logger:set_primary_config(level, notice),
  {ok, _} = application:ensure_all_started(erlang_analyzer),
  logger:notice("Application booted"),
  try init(BasePath) of
      _ -> ok
  catch
    Type1:Error1:Trace1 ->
      logger:error("An error occured during start: ~p:~p~n~p", [Type1, Error1, Trace1]),
      erlang:halt(2)
  end,
  logger:notice("Analyzers configured"),
  try analyze(BasePath, Exercise) of
      _ -> ok
  catch
    Type2:Error2:Trace2 ->
      logger:error("An error occured during analysis: ~p:~p~n~p", [Type2, Error2, Trace2]),
      erlang:halt(3)
  end,
  timer:sleep(5000),
  erlang:halt(0).

init(BasePath) -> init(BasePath, ?ALL_LINTERS).

init(BasePath, Linters) when is_list(Linters) ->
  Files = filelib:wildcard("*.erl", BasePath),
  logger:notice("Files: ~p", [Files]),
  FileLinter =
    lists:flatmap(fun (File) ->
                      lists:map(fun (Linter) -> {File, Linter} end, Linters)
                  end, Files),
  logger:notice("FileLinters: ~p", [FileLinter]),
  lists:map(fun ({File, Linter}) -> erlang_analyzer_linter:prepare(Linter, File) end,
            FileLinter).

analyze(BasePath, File) ->
  FullPath = filename:join(BasePath, File),
  case file:open(FullPath, [read]) of
    {ok, FD} ->
      analyze(BasePath, File, FD);
    {error, Reason} ->
      logger:error("Unable to open file \"~s\": ~p", [FullPath, Reason]),
      {error, Reason}
  end.

analyze(BasePath, File, FD) ->
  analyze(BasePath, File, FD, ?ALL_LINTERS).

analyze(BasePath, File, FD, Linters) when is_list(Linters) ->
  {ok, Forms}  = epp_dodger:parse(FD),
  logger:notice("Forms: ~p", [Forms]),
  {ok, #{export_all => []}}.

%%====================================================================
%% Internal functions
%%====================================================================
