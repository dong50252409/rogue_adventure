%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 是移动状态
%%% @end
%%%-------------------------------------------------------------------
-module(cond_is_move_state).

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
tick(_TreeNode, BB, #{state := UnitState} = State) when ?IS_STATE(?UNIT_STATE_MOVE, UnitState) ->
    {?BT_SUCCESS, BB, State};
tick(_TreeNode, BB, State) ->
    {?BT_FAILURE, BB, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------