%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 设置攻击目标
%%% @end
%%%-------------------------------------------------------------------
-module(action_set_attack_target).

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
tick(_TreeNode, BB, #{misc := #{atk_id := AtkId}} = State) ->
    case State of
        #{target_id := _} ->
            {?BT_SUCCESS, BB, State};
        #{misc := #{atk_id := AtkId} = Misc} ->
            UpMisc = maps:without(?IDLE_ERASE_MISC_KEY_LIST, Misc),
            UpState = maps:without(?IDLE_DELETE_KEY_LIST, State),
            {?BT_SUCCESS, BB, UpState#{target_id => AtkId, misc := UpMisc}}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------