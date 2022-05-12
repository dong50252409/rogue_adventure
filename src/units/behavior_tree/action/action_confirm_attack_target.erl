%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 确认攻击目标
%%% @end
%%%-------------------------------------------------------------------
-module(action_confirm_attack_target).

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
tick(_TreeNode, BB, #{target_id := TargetId} = State) ->
    case rogue_adventure_mgr:has_unit_info(TargetId) of
        ?true ->
            {?BT_SUCCESS, State};
        ?false ->
            tick(_TreeNode, BB, maps:remove(target_id, State))
    end;
tick(_TreeNode, BB, #{id := Id, type := UnitType, pos := Pos, attr := #{guard_range := GuardRange}} = State) ->
    TargetId = get_target_id(Pos, GuardRange, UnitType),
    ?DEBUG("~ts:确认攻击目标`~ts`", [unit:unit_name(Id), unit:unit_name(TargetId)]),
    {?BT_SUCCESS, BB, State#{target_id => TargetId}}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% 筛选目标id
get_target_id(Pos, GuardRange, UnitType) ->
    ConditionFun = get_condition_fun(UnitType),
    Fun =
        fun(Grid, Acc) ->
            case rogue_adventure_mgr:get_unit_info_by_pos(Grid) of
                ?undefined ->
                    Acc;
                #ets_unit_info{id = TargetId, type = TargetUnitType} ->
                    ?IF(ConditionFun(TargetUnitType), [TargetId | Acc], Acc)
            end
        end,
    [TargetId | _] = lists:foldl(Fun, [], lists:delete(Pos, util:grid_range(Pos, GuardRange))),
    TargetId.

%% 获取目标筛选条件
get_condition_fun(?UNIT_TYPE_ROGUE) ->
    fun(UnitType) -> ?IS_MONSTER(UnitType) end;
get_condition_fun(UnitType) when ?IS_MONSTER(UnitType) ->
    fun(?UNIT_TYPE_ROGUE) -> ?true;(_) -> ?false end.