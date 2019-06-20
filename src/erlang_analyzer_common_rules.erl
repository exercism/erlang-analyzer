-module(erlang_analyzer_common_rules).

-export([no_export_all/3]).

-type comment() :: #{
  rule := {atom(), atom()},
  line := pos_integer(),
  col  := pos_integer(),
  file := string() | binary()
}.

-type no_export_all_config() :: #{}.

-spec no_export_all(
  erlang_analyzer_config:config(),
  erlang_analyzer_helpers:file(),
  no_export_all_config())
-> [comment()].
no_export_all(Config, Target, _) ->
  {Tree, _} = erlang_analyzer_helpers:parse_tree(Config, Target),
  case find_export_all(Tree) of
    [] -> [];
    Locations when is_list(Locations) ->
      lists:map(fun (Location) -> complain(no_export_all, Location, Target) end, Locations)
  end.

find_export_all(#{content := Content}) ->
  FilterFun = fun
    (#{type := compile, attrs := #{value := export_all, location := Location}}) -> {true, Location};
    (_) -> false
  end,
  lists:filtermap(FilterFun, Content).

-spec complain(atom(), {pos_integer(), pos_integer()}, erlang_analyzer_helpers:file()) -> comment().
complain(Rule, {Line, Col}, File) ->
  #{
    rule => {?MODULE, Rule},
    line => Line,
    col  => Col,
    file => erlang_analyzer_helpers:filename(File)
  }.