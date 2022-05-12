%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 当前位置有宝箱
%%% @end
%%%-------------------------------------------------------------------
-module(cond_has_box).

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
    Fun = fun(Grid) -> rogue_adventure_mgr:get_maze_grid(Grid) =:= ?UNIT_TYPE_BOX end,
    case lists:any(Fun, util:grid_border(Pos, 1)) of
        ?true ->
            {?BT_SUCCESS, BB, State};
        ?false ->
            {?BT_FAILURE, BB, State}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------