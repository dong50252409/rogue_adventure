%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 等待恢复血量
%%% @end
%%%-------------------------------------------------------------------
-module(action_wait_recover).

%%--------------------------------------------------------------------
%% include
%%--------------------------------------------------------------------
-include("behavior3.hrl").
-include("rogue_adventure.hrl").
%%--------------------------------------------------------------------
%% export API
%%--------------------------------------------------------------------
-export([open/3, tick/3, close/3]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------
-spec open(tree_node(), blackboard(), term()) -> {blackboard(), term()}.
open(#tree_node{id = BId}, BB, #{attr := #{def := Def}} = State) ->
    {blackboard:set(recover_ts, util:now_msec() + erlang:trunc(1000 / Def * 1000) * 10, BId, BB), State}.

-spec tick(tree_node(), blackboard(), term()) -> {bt_status(), blackboard(), term()}.
tick(#tree_node{id = BId}, BB, #{id := Id, attr := #{max_hp := MaxHP, hp := HP} = Attr} = State) ->
    case util:now_msec() >= blackboard:get(recover_ts, BId, BB) of
        ?true ->
            UpState = State#{attr := Attr#{hp := MaxHP}},
            ?DEBUG("~ts:休息结束，恢复血量:~ts 可以继续探险了 当前血量:~ts 状态:~ts", [
                unit:unit_name(Id),
                color:greenb(erlang:integer_to_list(MaxHP - HP)),
                color:green(erlang:integer_to_list(MaxHP)),
                unit:get_health_state(MaxHP, MaxHP)]
            ),
            {?BT_SUCCESS, BB, UpState};
        ?false ->
            {?BT_RUNNING, BB, State}
    end.

-spec close(tree_node(), blackboard(), term()) -> {blackboard(), term()}.
close(#tree_node{id = BId}, BB, State) ->
    {blackboard:remove(recover_ts, BId, BB), State}.
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------