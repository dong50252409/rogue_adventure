%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 生成巡逻路径
%%% @end
%%%-------------------------------------------------------------------
-module(action_gen_patrol_path).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").
-include("rogue_adventure.hrl").
%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([tick/3]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
-spec tick(tree_node(), blackboard(), term()) -> {bt_status(), blackboard(), term()}.
tick(_TreeNode, BB, #{pos := Pos} = State) ->
    Offset = [{-1, -1}, {-1, 0}, {-1, 1}, {0, 1}, {1, 1}, {1, 0}, {1, -1}, {0, -1}],
    case gen_patrol_path(Pos, Offset) of
        [] ->
            {?BT_SUCCESS, BB, State#{path => []}};
        Grids ->
            Grid = lists:nth(util:rand(1, erlang:length(Grids)), Grids),
            {?BT_SUCCESS, BB, State#{path => [Grid]}}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
gen_patrol_path({X, Y}, [{XOffset, YOffset} | T]) ->
    Grid = {X + XOffset, Y + YOffset},
    case util:check_border(Grid) andalso not rogue_adventure_mgr:is_block(Grid) of
        ?true ->
            [Grid | gen_patrol_path({X, Y}, T)];
        ?false ->
            gen_patrol_path({X, Y}, T)
    end;
gen_patrol_path(_Grid, []) ->
    [].