%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 生成闪避路径
%%% @end
%%%-------------------------------------------------------------------
-module(action_gen_dodge_path).

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
tick(_TreeNode, BB, #{id := Id, pos := Pos, target_id := TargetId, attr := #{max_hp := MaxHP, hp := HP, atk_range := AtkRange}} = State)
    when HP * 100 div MaxHP >= 30 ->
    TargetPos = rogue_adventure_mgr:get_unit_info_by_index(TargetId, #ets_unit_info.pos),
    case AtkRange - util:distance(Pos, TargetPos) of
        Num when Num > 0 ->
            Path = gen_dodge_path(Pos, TargetPos),
            ?IF(Path =/= [],
                ?DEBUG("~ts:我闪，我闪，我闪闪闪 位置:~w", [unit:unit_name(Id), erlang:hd(Path)]),
                ?DEBUG("~ts:存爷们就得正面硬钢", [unit:unit_name(Id)])),
            {?BT_SUCCESS, BB, State#{path => Path}};
        _ ->
            ?DEBUG("~ts:存爷们就得正面硬钢", [unit:unit_name(Id)]),
            {?BT_SUCCESS, BB, State#{path => []}}
    end;

tick(_TreeNode, BB, #{id := Id, pos := Pos, target_id := TargetId} = State) ->
    TargetPos = rogue_adventure_mgr:get_unit_info_by_index(TargetId, #ets_unit_info.pos),
    Path = gen_dodge_path(Pos, TargetPos),
    ?IF(Path =/= [],
        ?DEBUG("~ts:要死了要死了，快跑 位置:~w", [unit:unit_name(Id), erlang:hd(Path)]),
        ?DEBUG("~ts:没有可躲的地方，难道要交代这了？", [unit:unit_name(Id)])),
    {?BT_SUCCESS, BB, State#{path => Path}}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

gen_dodge_path(StartGrid, TargetPos) ->
    case [{util:distance(TargetPos, Grid), Grid} || Grid <- util:grid_border(StartGrid, 1), not rogue_adventure_mgr:is_block(Grid)] of
        [] ->
            [];
        [{_, Grid} | _] ->
            [Grid]
    end.

