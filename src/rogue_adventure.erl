%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rogue_adventure).

-include("rogue_adventure.hrl").
%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
	ParseArgs = parse_args(Args),
	ok = application:start(rogue_adventure),
	{ok, _} = rogue_adventure_sup:start_child(rogue_adventure_mgr, [{parent_pid, self()} | ParseArgs]).

%%====================================================================
%% Internal functions
%%====================================================================
%% 解析输出参数
-spec parse_args(list()) -> ok|help.
parse_args(["--help" | _T]) ->
	help,
	erlang:halt();
parse_args(["--width", WidthStr | T]) ->
	[{width, erlang:list_to_integer(WidthStr)} | parse_args(T)];
parse_args(["--high", HighStr | T]) ->
	[{high, erlang:list_to_integer(HighStr)} | parse_args(T)];
parse_args([]) ->
	[].
