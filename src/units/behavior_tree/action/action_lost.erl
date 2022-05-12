%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 失败
%%% @end
%%%-------------------------------------------------------------------
-module(action_lost).

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
tick(_TreeNode, BB, #{id := Id} = State) ->
    ?DEBUG("~ts:我，我，我的财宝。。。", [unit:unit_name(Id)]),
    rogue_adventure_mgr:game_over(victory, "罗格失败了！！"),
    {?BT_SUCCESS, BB, State}.
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------