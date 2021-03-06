%% @author Norbert Melzer <nmelzer@nobbz.dev>
%% @doc Provides helpers to work with files.
%% Any function in this module does also accept a `string'/`binary' where a `file()' is expected.
%% To allow some sort of caching, it will also return a new `file()' which then encapsulates
%% metadata. Future reads of an exisitng attribute in the return `file()' will return faster.
%% @end
-module(ea_files).

-export([content/2, filename/2, full_path/2, parse_tree/2]).
-export_type([file/0, filedata/0]).

-type file() :: file:name_all()
              | filedata().
%% Represents a file in the project.

-opaque filedata() :: #{ name    := file:name_all()
                       , content => binary()
                       , tree    => ktn_code:tree_node()}.
%% Contains a files metadata.

-define(IS_NAME(Name), (is_binary(Name) orelse is_list(Name))).

%% @doc Parses the files content into a representation that we can analyze.
%% @param Config the project configuration.
%% @param File   the filename or metadata.
%% @returns a tuple with the {@link parse_tree()} as its first and the updated
%%    {@link filedata()} as the second element.
-spec parse_tree(
  Config :: ea_config:config(),
  File   :: file()
) -> {ktn_code:tree_node(), filedata()}.
parse_tree(_, File = #{tree := Tree}) ->
  {Tree, File};
parse_tree(Config, File) ->
  {Content, File1} = content(Config, File),
  Tree  = ktn_code:parse_tree(Content),
  File2 = put_field(tree, Tree, File1),
  parse_tree(Config, File2).

%% @doc Reads the files content from disk.
%% @param Config the project configuration.
%% @param File   the filename or metadata.
%% @returns a tuple with a `binary()' representing the files content as read from disk as its first
%%    and the updated {@link filedata()} as its second element.
-spec content(
  Config :: ea_config:config(),
  File   :: file()
) -> {binary(), file()}.
content(_Config, File = #{content := Content}) ->
  {Content, File};
content(Config, File) ->
  FullPath        = full_path(Config, File),
  {ok, Content}   = file:read_file(FullPath),
  FileWithContent = put_field(content, Content, File),
  content(Config, FileWithContent).

%% @doc Returns the full path of the file.
%% @param Config the project configuration.
%% @param File   the filename or metadata.
%% @returns the absolute path to the file on disk.
-spec full_path(
  Config :: ea_config:config(),
  File   :: file()
) -> file:filename_all().
full_path(Config = #{project_path := Base}, File) ->
  filename:join(Base, filename(Config, File)).

%% @doc Returns the files name in the project.
%% @param Config the project configuration.
%% æparam File   the filename or metadata.
%% @returns the filename relative to the project root.
-spec filename(
  Config :: ea_config:config(),
  File   :: file()
) -> file:name_all().
filename(_Config, File) when ?IS_NAME(File) -> File;
filename(_Config, #{name := Name}) -> Name.

-spec put_field(Key :: name,    Value :: file:name_all(),      File :: file()) -> filedata()
             ; (Key :: content, Value :: binary(),             File :: file()) -> filedata()
             ; (Key :: tree,    Value :: ktn_code:tree_node(), File :: file()) -> filedata().
put_field(Key, Value, File = #{}) ->
  maps:put(Key, Value, File);
put_field(Key, Value, File) when ?IS_NAME(File) ->
  put_field(Key, Value, #{name => File}).
