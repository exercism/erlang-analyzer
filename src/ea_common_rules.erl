-module(ea_common_rules).

-export([all/0]).
-export([no_export_all/3, no_test_version/3, use_ignored_variable/3]).

-define(COMPLAIN(Config, Location, Target), complain(Config, ?FUNCTION_NAME, Location, Target)).

-type comment() :: #{
  rule := {atom(), atom()},
  line := pos_integer(),
  col  := pos_integer(),
  file := string() | binary()
}.

-type no_export_all_config()        :: #{}.
-type no_test_version_config()      :: #{}.
-type use_ignored_variable_config() :: #{}.

-spec all() -> [{atom(), atom(), map()}].
all() ->
  lists:map(fun ({F, O}) -> {?MODULE, F, O} end, [
    {no_export_all, #{}},
    {no_test_version, #{}}
  ]).

-spec no_export_all(
  ea_config:config(),
  ea_files:file(),
  no_export_all_config())
-> [comment()].
no_export_all(Config, Target, _) ->
  {Tree, _} = ea_files:parse_tree(Config, Target),
  case find_export_all(Tree) of
    [] -> [];
    Locations when is_list(Locations) ->
      lists:map(fun (Location) -> complain(Config, no_export_all, Location, Target) end, Locations)
  end.

find_export_all(#{content := Content}) ->
  FilterFun = fun
    (#{type := compile, attrs := #{value := export_all, location := Location}}) -> {true, Location};
    (_) -> false
  end,
  lists:filtermap(FilterFun, Content).

-spec no_test_version(
  ea_config:config(),
  ea_files:file(),
  no_test_version_config()
) -> [comment()].
no_test_version(Config, Target, _) ->
  {Tree, _} = ea_files:parse_tree(Config, Target),
  case find_test_version(Tree) of
    [] -> [];
    Locations when is_list(Locations) ->
      lists:map(fun (Location) -> ?COMPLAIN(Config, Location, Target) end, Locations)
  end.

find_test_version(#{content := Content}) ->
  FilterFun = fun
    (#{type := export, attrs := #{value := Es, location := Loc}}) ->
      case lists:member({test_version, 0}, Es) of
        true -> {true, Loc};
        false -> false
      end;
    (_) -> false
  end,
  lists:filtermap(FilterFun, Content).

-spec use_ignored_variable(
  ea_config:config(),
  ea_files:file(),
  use_ignored_variable_config()
) -> [comment()].
use_ignored_variable(Config, Target, _) ->
  {Tree, _} = ea_files:parse_tree(Config, Target),
  case elvis_code:find(fun is_ignored_var/1, Tree, #{mode => zipper}) of
    [] -> [];
    UsedIgnoredVars ->
      Locations = lists:map(fun (#{attrs := #{location := L}}) -> L end, UsedIgnoredVars),
      lists:map(fun (Location) -> ?COMPLAIN(Config, Location, Target) end, Locations)
  end.

is_ignored_var(Zipper) ->
  Node = zipper:node(Zipper),
  case ktn_code:type(Node) of
    var ->
      Name = ktn_code:attr(name, Node),
      [FirstChar|_] = atom_to_list(Name),
      (FirstChar =:= $_) andalso (Name =/= '_') andalso not check_parent_match(Zipper);
    _ -> false
  end.

check_parent_match(Zipper) ->
  case zipper:up(Zipper) of
    undefined -> false;
    ParentZipper ->
      Parent = zipper:node(ParentZipper),
      case ktn_code:type(Parent) of
        match ->
          zipper:down(ParentZipper) == Zipper;
        _ -> check_parent_match(ParentZipper)
      end
  end.

-spec complain(
  ea_config:config(),
  atom(),
  {pos_integer(), pos_integer()},
  ea_files:file()
) -> comment().
complain(Config, Rule, {Line, Col}, File) ->
  #{
    rule => {?MODULE, Rule},
    line => Line,
    col  => Col,
    file => ea_files:filename(Config, File)
  }.