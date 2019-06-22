-module(ea_files).

-export([content/2, filename/2, full_path/2, parse_tree/2]).
-export_type([file/0, name/0, tree_node/0]).

%% TODO: fix type of `tree` field once https://github.com/inaka/katana-code/issues/27 got fixed.
-type tree_node() :: map().

-type name() :: string() | binary().

-type file() :: string()
              | binary()
              | #{ name    := name()
                 , content => binary()
                 , tree    => tree_node()}.

-define(IS_NAME(Name), (is_binary(Name) orelse is_list(Name))).

%% TODO: use proper type where we have term now
-spec parse_tree(ea_config:config(), file()) -> {tree_node(), file()}.
parse_tree(_, File = #{tree := Tree}) ->
  {Tree, File};
parse_tree(Config, File) ->
  {Content, File1} = content(Config, File),
  Tree  = ktn_code:parse_tree(Content),
  File2 = put_field(tree, Tree, File1),
  parse_tree(Config, File2).

-spec content(ea_config:config(), file()) -> {binary(), file()}.
content(_Config, File = #{content := Content}) ->
  {Content, File};
content(Config, File) ->
  FullPath        = full_path(Config, File),
  {ok, Content}   = file:read_file(FullPath),
  FileWithContent = put_field(content, Content, File),
  content(Config, FileWithContent).

-spec full_path(ea_config:config(), file()) -> file:filename_all().
full_path(Config = #{project_path := Base}, File) ->
  filename:join(Base, filename(Config, File)).

-spec filename(ea_config:config(), file()) -> name().
filename(_Config, File) when ?IS_NAME(File) -> File;
filename(_Config, #{name := Name}) -> Name.

-spec put_field(name,    name(),      file()) -> file()
             ; (content, binary(),    file()) -> file()
             ; (tree,    tree_node(), file()) -> file().
put_field(Key, Value, File = #{}) ->
  maps:put(Key, Value, File);
put_field(Key, Value, File) when ?IS_NAME(File) ->
  put_field(Key, Value, #{name => File}).
