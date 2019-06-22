-module(ea_files).

-export([filename/2, parse_tree/2]).
-export_type([file/0, tree_node/0]).

%% TODO: fix type of `tree` field once https://github.com/inaka/katana-code/issues/27 got fixed.
-type tree_node() :: map().

-type file() :: string()
              | binary()
              | #{ name    := string() | binary()
                 , content => binary()
                 , tree    => tree_node()}.

%% TODO: use proper type where we have term now
-spec parse_tree(ea_config:config(), file()) -> {tree_node(), file()}.
parse_tree(_, File = #{tree := Tree}) ->
  {Tree, File};
parse_tree(Config, File = #{content := Content}) ->
  Tree  = ktn_code:parse_tree(Content),
  File1 = maps:put(tree, Tree, File),
  parse_tree(Config, File1);
parse_tree(Config = #{project_path := Base}, File) when is_list(File); is_binary(File) ->
  FullPath = filename:join(Base, File),
  {ok, Content} = file:read_file(FullPath),
  parse_tree(Config, #{name => File, content => Content}).

-spec filename(ea_config:config(), file()) -> string() | binary().
filename(_Config, File) when is_binary(File); is_list(File) -> File;
filename(_Config, #{name := Name}) -> Name.