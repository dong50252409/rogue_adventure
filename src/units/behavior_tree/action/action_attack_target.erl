%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 攻击目标
%%% @end
%%%-------------------------------------------------------------------
-module(action_attack_target).

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
tick(_TreeNode, BB, #{id := Id, target_id := TargetId, attr := Attr} = State) ->
    Reply = unit:attack_target(TargetId, Id, Attr),
    UpAttr = Attr#{cd := util:now_msec() + erlang:trunc(1000 / maps:get(spd, Attr) * 1000)},
    UpState = State#{attr := UpAttr},
    case Reply of
        died ->
            {?BT_SUCCESS, BB, UpState};
        alive ->
            {?BT_FAILURE, BB, UpState}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------