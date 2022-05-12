%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 警戒范围内有目标
%%% @end
%%%-------------------------------------------------------------------
-module(cond_has_guard_attack_target).

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
tick(_TreeNode, BB, #{type := ?UNIT_TYPE_ROGUE, pos := Pos, attr := #{guard_range := GuardRange}} = State) ->
    Fun = fun(Grid) -> ?IS_MONSTER(rogue_adventure_mgr:get_maze_grid(Grid)) end,
    case lists:any(Fun, lists:delete(Pos, util:grid_range(Pos, GuardRange))) of
        ?true ->
            {?BT_SUCCESS, BB, State};
        ?false ->
            {?BT_FAILURE, BB, State}
    end;
tick(_TreeNode, BB, #{type := UnitType, pos := Pos, attr := #{guard_range := GuardRange}} = State) when ?IS_MONSTER(UnitType) ->
    Fun = fun(Grid) -> rogue_adventure_mgr:get_maze_grid(Grid) =:= ?UNIT_TYPE_ROGUE end,
    case lists:any(Fun, lists:delete(Pos, util:grid_range(Pos, GuardRange))) of
        ?true ->
            {?BT_SUCCESS, BB, State};
        ?false ->
            {?BT_FAILURE, BB, State}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------