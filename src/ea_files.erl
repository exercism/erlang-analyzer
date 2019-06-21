-module(ea_files).

-export([filename/1, parse_tree/2]).

-type file() :: string()
              | binary()
              | #{ name    := string() | binary()
                 , content => binary()
                 , tree    => ktn_code:tree_node()}.

%% TODO: use proper type where we have term now
-spec parse_tree(term(), file()) -> {ktn_code:tree_node(), file()}.
parse_tree(_, File = #{tree := Tree}) ->
  {Tree, File};
parse_tree(Config, File = #{content := Content}) ->
  Tree = ktn_code:parse_tree(Content),
  File1 = maps:put(tree, Tree, File),
  parse_tree(Config, File1);
parse_tree(Config = #{project_path := Base}, File) when is_list(File); is_binary(File) ->
  FullPath = filename:join(Base, File),
  {ok, Content} = file:read_file(FullPath),
  parse_tree(Config, #{name => File, content => Content}).

filename(File) when is_binary(File); is_list(File) -> File;
filename(#{name := Name}) -> Name.