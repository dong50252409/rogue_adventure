%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 解锁宝箱
%%% @end
%%%-------------------------------------------------------------------
-module(action_unlock_box).

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
tick(_TreeNode, BB, #{id := Id, attr := #{spd := Spd, cd := CD} = Attr, misc := #{box_list := BoxList, box_target_id := BoxId} = Misc} = State) ->
    case util:now_msec() >= CD of
        ?true ->
            Reply = unit:unlock_box(BoxId, Id, Attr),
            UpAttr = Attr#{cd := util:now_msec() + Spd},
            case Reply of
                already_unlock ->
                    UpBoxList = lists:keydelete(BoxId, 1, BoxList),
                    UpMisc = maps:remove(box_target_id, Misc),
                    {?BT_SUCCESS, BB, State#{attr := UpAttr, misc := UpMisc#{box_list := UpBoxList}}};
                continue ->
                    {?BT_RUNNING, BB, State#{attr := UpAttr}}
            end;
        ?false ->
            {?BT_RUNNING, BB, State}
    end.
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------