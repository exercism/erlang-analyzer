-module(erlang_analyzer).

%% API exports
-export([main/1, start/0, start/1, analyze/2, analyze/3]).

-define(ALL_LINTERS,
        [export_all]).

%%====================================================================
%% API functions
%%====================================================================

main([Exercise, BasePath]) ->
  {ok, _} = application:ensure_all_started(erlang_analyzer),
  io:format("Application booted~n"),
  try init(BasePath) of
      _ -> ok
  catch
    Type1:Error1:Trace1 ->
      io:format("An error occured during start: ~p:~p~n~p~n", [Type1, Error1, Trace1]),
      erlang:halt(2)
  end,
  try analyze(BasePath, Exercise) of
      _ -> ok
  catch
    Type2:Error2:Trace2 ->
      io:format("An error occured during analysis: ~p:~p~n~p", [Type2, Error2, Trace2]),
      erlang:halt(3)
  end,
  timer:sleep(5000),
  erlang:halt(0).

start() -> start(?ALL_LINTERS).

start(Linters) when is_list(Linters) ->
  io:format("supervisor result: ~p~n", [erlang_analyzer_sup:start_link(Linters)]).

init(BasePath) -> init(BasePath, ?ALL_LINTERS).

init(BasePath, Linters) when is_list(Linters) ->
  Files = filelib:wildcard("*.erl", BasePath),
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
  case file:open(FullPath, [read]) of
    {ok, FD} ->
      analyze(BasePath, File, FD);
    {error, Reason} ->
      io:format("Unable to open file \"~s\": ~p~n", [FullPath, Reason]),
      {error, Reason}
  end.

analyze(BasePath, File, FD) ->
  analyze(BasePath, File, FD, ?ALL_LINTERS).

analyze(BasePath, File, FD, Linters) when is_list(Linters) ->
  {ok, Forms}  = epp_dodger:parse(FD),
  io:format("Forms: ~p~n", [Forms]),
  {ok, #{export_all => []}}.

%%====================================================================
%% Internal functions
%%====================================================================
