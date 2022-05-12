%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 是可移动格子
%%% @end
%%%-------------------------------------------------------------------
-module(cond_is_empty_grid).

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
tick(_TreeNode, BB, #{path := Path} = State) ->
    case Path of
        [Grid | _] ->
            case not rogue_adventure_mgr:is_block(Grid) andalso not rogue_adventure_mgr:has_unit_info(Grid) of
                ?true ->
                    {?BT_SUCCESS, BB, State};
                ?false ->
                    {?BT_FAILURE, BB, State}
            end;
        [] ->
            {?BT_FAILURE, BB, State}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------