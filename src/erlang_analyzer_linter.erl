-module(erlang_analyzer_linter).

-callback init(string()) -> {ok, term()} | {error, term()}.
-callback handle_form(term(), term()) -> {ok, term()}.
-callback get_lints(term()) -> {ok, list(term())}.

-export([start_link/1, prepare/2, check/3, get/2]).
-export([init/1, handle_cast/2, handle_call/3]).

-record(state, {name, module, filestate = #{}}).

start_link(Linter) ->
  logger:notice("Starting linter \"~s\"", [Linter]),
  LinterModule = list_to_atom("erlang_analyzer_linter_" ++ atom_to_list(Linter)),
  try gen_server:start({local, Linter}, ?MODULE, {Linter, LinterModule}, []) of
      A -> A
  catch
    T:E:S ->
      logger:error("Failed starting linter \"~s\":~n~p:~p~n~p", [Linter, T, E, S])
  end.

prepare(Linter, File) ->
  gen_server:cast(Linter, {prepare, File}).

check(Linter, File, Form) ->
  gen_server:cast(Linter, {check, File, Form}).

get(Linter, File) ->
  gen_server:call(Linter, {get, File}, infinity).

init({Name, Module}) ->
  logger:notice("Initialising linter \"~p\"", [Name]),
  State = #state{name = Name, module = Module},
  {ok, State}.

handle_call({get, File}, _, State) ->
  {reply, [], State}.

handle_cast({prepare, File}, State) ->
  Module   = State#state.module,
  FileData = State#state.filestate,
  case Module:init(File) of
    {ok, LinterState} ->
      FileData2 = maps:put(File, LinterState, FileData),
      {noreply, State#state{filestate = FileData2}};
    {error, Reason} ->
      logger:error("Error: ~p~n~p", [File, Reason]),
      erlang:throw('Im not yet sure how to deal with this')
  end;
handle_cast({check, File, Form}, State) ->
  Module   = State#state.module,
  FileData = State#state.filestate,
  case Module:handle_form(Form, FileData) of
    {ok, LinterState} ->
      FileData2 = maps:put(File, LinterState, FileData),
      {noreply, State#state{filestate = FileData2}}
  end.
