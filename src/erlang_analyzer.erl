-module(erlang_analyzer).

%% API exports
-export([main/1]).

-define(ALL_LINTERS,
        [export_all]).

%%====================================================================
%% API functions
%%====================================================================

-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(erlang_analyzer),
  ok.

main([_Exercise, _BasePath]) ->
  ok = logger:set_primary_config(level, notice),
  ok = start(),
  logger:notice("Application booted"),
  % try init(BasePath) of
  %     _ -> ok
  % catch
  %   Type1:Error1:Trace1 ->
  %     logger:error("An error occured during start: ~p:~p~n~p", [Type1, Error1, Trace1]),
  %     erlang:halt(2)
  % end,
  % logger:notice("Analyzers configured"),
  % try analyze(BasePath, Exercise) of
  %     Data ->
  %       logger:notice("~p", [Data]), ok
  % catch
  %   Type2:Error2:Trace2 ->
  %     logger:error("An error occured during analysis: ~p:~p~n~p", [Type2, Error2, Trace2]),
  %     timer:sleep(100),
  %     erlang:halt(3)
  % end,
  timer:sleep(250),
  erlang:halt(0).

% init(BasePath) -> init(BasePath, ?ALL_LINTERS).

% init(BasePath, Linters) when is_list(Linters) ->
%   Files = filelib:wildcard("*.erl", BasePath),
%   logger:notice("Files: ~p", [Files]),
%   FileLinter =
%     lists:flatmap(fun (File) ->
%                       lists:map(fun (Linter) -> {File, Linter} end, Linters)
%                   end, Files),
%   logger:notice("FileLinters: ~p", [FileLinter]),
%   lists:map(fun ({File, Linter}) -> erlang_analyzer_linter:prepare(Linter, File) end,
%             FileLinter).

% analyze(BasePath, File) ->
%   File1 = lists:map(fun
%     ($-) -> $_;
%     (C) -> C end, File) ++ ".erl",
%   FullPath = filename:join(BasePath, File1),
%   case file:read_file(FullPath) of
%     {ok, Content} ->
%       analyze(BasePath, File1, Content);
%     {error, Reason} ->
%       logger:error("Unable to open file \"~s\": ~p", [FullPath, Reason]),
%       {error, Reason}
%   end.

% analyze(BasePath, File, Content) ->
%   analyze(BasePath, File, Content, ?ALL_LINTERS).

% analyze(BasePath, File, Content, Linters) when is_list(Linters) ->
%   Forms = ktn_code:parse_tree(Content),
%   logger:notice("Forms: ~p", [Forms]),
%   {ok, #{export_all => []}}.

%%====================================================================
%% Internal functions
%%====================================================================
