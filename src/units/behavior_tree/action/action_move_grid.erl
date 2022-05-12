%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 移动一格
%%% @end
%%%-------------------------------------------------------------------
-module(action_move_grid).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").
-include_lib("rogue_adventure.hrl").
%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([open/3, tick/3, close/3]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
-spec open(tree_node(), blackboard(), term()) -> {blackboard(), term()}.
open(#tree_node{id = BId}, BB, #{attr := #{mov := Mov}} = State) ->
    {blackboard:set(move_ts, util:now_msec() + erlang:trunc(1000 / Mov * 1000), BId, BB), State}.

-spec tick(tree_node(), blackboard(), term()) -> {bt_status(), blackboard(), term()}.
tick(#tree_node{id = BId}, BB, #{id := Id, type := UnitType, pos := Pos, path := [Grid | T]} = State) ->
    case util:now_msec() >= blackboard:get(move_ts, BId, BB) of
        ?true ->
            rogue_adventure_mgr:update_maze_row(Pos, ?UNIT_TYPE_EMPTY),
            rogue_adventure_mgr:update_maze_row(Grid, UnitType),
            rogue_adventure_mgr:update_unit_info(Id, [{#ets_unit_info.pos, Grid}]),
            {?BT_SUCCESS, BB, State#{pos := Grid, path := T}};
        ?false ->
            {?BT_RUNNING, BB, State}
    end.

-spec close(tree_node(), blackboard(), term()) -> {blackboard(), term()}.
close(#tree_node{id = BId}, BB, State) ->
    {blackboard:remove(move_ts, BId, BB), State}.
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------