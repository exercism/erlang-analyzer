-module(erlang_analyzer_linter).

-callback init(string()) -> {ok, term()} | {error, term()}.
-callback handle_form(term()) -> {ok, term()}.
-callback get_lints(term()) -> {ok, list(term())}.

-export([start_link/1]).
-export([init/1]).

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

init({Name, Module}) ->
  io:format("Initialising linter \"~p\"~n", [Name]),
  State = #state{name = Name, module = Module},
  {ok, State}.
