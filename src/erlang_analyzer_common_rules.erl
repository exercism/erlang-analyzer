-module(erlang_analyzer_common_rules).

-export([no_export_all/3]).

no_export_all(Config, Target, _) ->
  {Tree, _} = erlang_analyzer_helpers:parse_tree(Config, Target),
  case find_export_all(Tree) of
    [] -> [];
    Locations when is_list(Locations) ->
      lists:map(fun (Location) -> complain(no_export_all, Location, Target) end, Locations)
  end.

find_export_all(#{content := Content}) ->
  CompileAttrs   = lists:filter(fun (#{type  := compile})                 -> true; (_) -> false end, Content),
  ExportAllAttrs = lists:filter(fun (#{attrs := #{value := export_all}})  -> true; (_) -> false end, CompileAttrs),
  _              = lists:map(   fun (#{attrs := #{location := Location}}) -> Location           end, ExportAllAttrs).

complain(Rule, {Line, Col}, File) ->
  #{
    rule => Rule,
    line => Line,
    col  => Col,
    file => erlang_analyzer_helpers:filename(File)
  }.