-module(erlang_analyzer_linter_two_fer).

-behaviour(erlang_analyzer_linter).

-export([init/1, handle_form/2]).

init(File) ->
  io:format("INit 'two_fer' for ~p~n", [File]),
  {ok, []}.

handle_form({tree,attribute,_,_} = Form, State) ->
  case erl_syntax:atom_value(erl_syntax:attribute_name(Form)) of
    module -> {ok, State};
    export -> {ok, State};
    _ -> throw({unhandable, Form, erl_prettypr:format(Form)})
  end;
handle_form({tree,function,_,_} = Form, State) ->
  {ok, State};
handle_form(Form, State) ->
  throw({unhandable, Form, erl_prettypr:format(Form)}).
