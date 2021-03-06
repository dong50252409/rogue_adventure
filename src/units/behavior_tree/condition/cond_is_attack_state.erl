%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 是攻击状态
%%% @end
%%%-------------------------------------------------------------------
-module(cond_is_attack_state).

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
tick(_TreeNode, BB, #{state := UnitState} = State) when ?UNIT_STATE_ATTACK =:= UnitState ->
    {?BT_SUCCESS, BB, State};
tick(_TreeNode, BB, State) ->
    {?BT_FAILURE, BB, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------