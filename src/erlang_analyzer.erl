-module(erlang_analyzer).

%% API exports
-export([main/1, analyze/2]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
  io:format("Args: ~p~n", [Args]),
  erlang:halt(0).

analyze(_Name, _Code) ->
  {ok, #{export_all => true}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% Local Variables:
%% erlang-indent-level: 2
%% End:
