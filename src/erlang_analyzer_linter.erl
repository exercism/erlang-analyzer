-module(erlang_analyzer_linter).

-callback init(string()) -> {ok, term()} | {error, term()}.
-callback handle_form(term()) -> {ok, term()}.
-callback get_lints(term()) -> {ok, list(term())}.

-export([start_link/1, prepare/2]).
-export([init/1, handle_cast/2]).

-record(state, {name, module, filestate = #{}}).

start_link(Linter) ->
  io:format("Starting linter \"~s\"~n", [Linter]),
  LinterModule = list_to_atom("erlang_analyzer_linter_" ++ atom_to_list(Linter)),
  try gen_server:start({local, Linter}, ?MODULE, {Linter, LinterModule}, []) of
      A -> A
  catch
    T:E:S ->
      io:format("Failed starting linter \"~s\":~n~p:~p~n~p~n", [Linter, T, E, S])
  end.

prepare(Linter, File) ->
  gen_server:cast(Linter, {prepare, File}).

init({Name, Module}) ->
  io:format("Initialising linter \"~p\"~n", [Name]),
  State = #state{name = Name, module = Module},
  {ok, State}.

handle_cast({prepare, File}, State) ->
  Module   = State#state.module,
  FileData = State#state.filestate,
  case Module:init(File) of
    {ok, LinterState} ->
      FileData2 = maps:put(File, LinterState, FileData),
      {noreply, State#state{filestate = FileData2}};
    {error, Reason} ->
      io:format("Error: ~p~n~p~n", [File, Reason]),
      erlang:throw('Im not yet sure how to deal with this')
  end.
