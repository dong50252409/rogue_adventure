%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 目标在警戒范围内
%%% @end
%%%-------------------------------------------------------------------
-module(cond_is_target_guard_range).

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
tick(_TreeNode, BB, #{pos := Pos, target_id := TargetId, attr := #{guard_range := GuardRange}} = State) ->
    case rogue_adventure_mgr:get_unit_info_by_index(TargetId, #ets_unit_info.pos) of
        ?undefined ->
            {?BT_FAILURE, BB, State};
        TargetPos ->
            case GuardRange >= util:distance(Pos, TargetPos) of
                ?true ->
                    {?BT_SUCCESS, BB, State};
                ?false ->
                    {?BT_FAILURE, BB, State}
            end
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------