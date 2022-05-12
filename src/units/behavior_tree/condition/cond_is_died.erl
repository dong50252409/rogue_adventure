%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 已死亡
%%% @end
%%%-------------------------------------------------------------------
-module(cond_is_died).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").
%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([tick/3]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
-spec tick(tree_node(), blackboard(), term()) -> {bt_status(), blackboard(), term()}.
tick(_TreeNode, BB, #{attr := #{hp := HP}} = State) when HP == 0 ->
    {?BT_SUCCESS, BB, State};
tick(_TreeNode, BB, State) ->
    {?BT_FAILURE, BB, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------