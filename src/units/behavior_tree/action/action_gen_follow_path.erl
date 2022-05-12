%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 生成怪物追击路径
%%% @end
%%%-------------------------------------------------------------------
-module(action_gen_follow_path).

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
tick(_TreeNode, BB, #{id := Id, type := UnitType, pos := Pos, target_id := TargetId} = State) ->
    TargetPos = rogue_adventure_mgr:get_unit_info_by_index(TargetId, #ets_unit_info.pos),
    Path = gen_follow_path(Pos, TargetPos),
    log(Id, TargetId, UnitType),
    {?BT_SUCCESS, BB, State#{path => Path}}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

gen_follow_path(StartGrid, TargetPos) ->
    case [{util:distance(StartGrid, Grid), Grid} || Grid <- util:grid_border(TargetPos, 1), not rogue_adventure_mgr:is_block(Grid)] of
        [] ->
            [];
        [{_, Grid} | _] ->
            [Grid]
    end.

log(Id, TargetId, ?UNIT_TYPE_ROGUE) ->
    ?DEBUG("~ts:一只`~ts`，别想跑", [unit:unit_name(Id), unit:unit_name(TargetId)]);
log(Id, TargetId, _) ->
    ?DEBUG("~ts:好不容易见到新鲜的人类，别想跑`~ts`", [unit:unit_name(Id), unit:unit_name(TargetId)]).