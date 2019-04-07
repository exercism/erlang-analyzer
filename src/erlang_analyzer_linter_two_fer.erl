-module(erlang_analyzer_linter_two_fer).

-behaviour(erlang_analyzer_linter).

-export([init/1, handle_form/2]).

init(File) ->
  io:format("INit 'two_fer' for ~p~n", [File]),
  {ok, []}.

%% handle_form({tree,attribute,_,_} = Form, State) ->
%%   case erl_syntax:atom_value(erl_syntax:attribute_name(Form)) of
%%     module -> {ok, State};
%%     export -> {ok, State};
%%     _ -> throw({unhandable, Form, erl_prettypr:format(Form)})
%%   end;
%% handle_form({tree,function,_,_} = Form, State) ->
%%   {ok, State};
handle_form(Form, State) ->
  case erl_syntax:type(Form) of
    attribute -> handle_attributes(Form, State);
    function  -> handle_function(Form, State);
    _ -> throw({unhandable, Form, erl_prettypr:format(Form), erl_syntax:type(Form)})
  end.

handle_attributes(Form, State) ->
  case erl_syntax_lib:analyze_attribute(Form) of
    {module, _} -> {ok, State};
    {export, Exports} -> analyze_exports(Exports, erl_syntax:get_pos(Form), State);
    Result -> throw({unknown_attribute, Result})
  end.

handle_function(Form, State) ->
  case erl_syntax_lib:analyze_function(Form) of
    {two_fer, 0} -> check_two_fer_0(Form, State);
    {two_fer, 1} -> check_two_fer_1(Form, State);
    R -> throw({unknown_function, R})
  end.

analyze_exports([], _, State) -> {ok, State};
analyze_exports([{two_fer, 0}|T], Line, State) -> analyze_exports(T, Line, State);
analyze_exports([{two_fer, 1}|T], Line, State) -> analyze_exports(T, Line, State);
analyze_exports([FA|T], Line, State) -> analyze_exports(T, Line, [{unneeded_export, FA, Line}|State]).

check_two_fer_0(Form, State) ->
  %% Zeroarity function always has exactly one clause
  [Clause] = erl_syntax:function_clauses(Form),
  case erl_syntax:clause_body(Clause) of
    [Expr] ->
      case {erl_syntax:application_operator(Expr),
            erl_syntax:application_arguments(Expr)} of
        {MF, [Arg]} ->
          case {erl_syntax:type(MF),
                erl_syntax:type(Arg)} of
            {atom, string} ->
              case {erl_syntax:atom_value(MF),
                    erl_syntax:string_value(Arg)} of
                {two_fer, "you"} -> {ok, State};
                Unknown  -> throw({two_fer_0, unknown, MF, Arg, Unknown})
              end
          end
      end;
    Body -> throw({two_fer_0, Body})
  end.

check_two_fer_1(Form, State) ->
  case erl_syntax:function_clauses(Form) of
    [Clause] ->
      case erl_syntax:clause_body(Clause) of
        _ -> {ok, State}; %% TODO: actually deal with this
        R -> throw({two_fer_1, unknown, R, erl_prettypr:format(Clause)})
      end
  end.
