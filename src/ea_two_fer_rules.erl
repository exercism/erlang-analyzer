-module(ea_two_fer_rules).

-export([all/0]).
-export([flatten_flat_list/3]).

-define(COMPLAIN(Config, Location, Target), complain(Config, ?FUNCTION_NAME, Location, Target)).

-type comment() :: #{
  rule := {atom(), atom()},
  line := pos_integer(),
  col  := pos_integer(),
  file := string() | binary()
}.

-type flatten_flat_list_config() :: #{}.

-spec all() -> [{atom(), atom(), map()}].
all() ->
  lists:map(fun ({F, O}) -> {?MODULE, F, O} end, [
    {flatten_flat_list, #{}}
  ]).

-spec flatten_flat_list(
  ea_config:config(),
  ea_files:file(),
  flatten_flat_list_config()
) -> [comment()].
flatten_flat_list(Config, File, _) ->
  {Tree, _} = ea_files:parse_tree(Config, File),
  Funs = find_function_defs(Tree),
  logger:error("~p", [Funs]).

find_function_defs(Tree = #{content := Content}) ->
  Exported  = find_exports(Tree),
  lists:filtermap(find_function_defs_filter(Exported), Content).

find_function_defs_filter(Exported) ->
  fun (Node) ->
    case ktn_code:type(Node) =:= function of
      true ->
        #{attrs := #{name := F, arity := A}} = Node,
        {true, #{
          exported => lists:member({F, A}, Exported),
          name     => F,
          arity    => A,
          node     => Node
        }};
      false ->
        false
    end
  end.

find_exports(#{content := Content}) ->
  FilterFun = fun
    (#{type := export, attrs := #{value := Exports}}) -> {true, Exports};
    (_) -> false
  end,
  Exported  = lists:filtermap(FilterFun, Content),
  lists:flatten(Exported).