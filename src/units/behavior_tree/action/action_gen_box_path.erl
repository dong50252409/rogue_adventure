%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 生成宝箱路径
%%% @end
%%%-------------------------------------------------------------------
-module(action_gen_box_path).

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
tick(_TreeNode, BB, #{id := Id, pos := Pos, misc := #{box_list := BoxList} = Misc} = State) ->
    {BoxId, Grid} = get_closest_box(BoxList, Pos),
    case astar:search(Pos, Grid, util:valid_fun(), [{max_limit, 16#FFFFFF}]) of
        {max, Path} ->
            ?DEBUG("~ts:探寻到宝箱准确位置 位置:~w", [unit:unit_name(Id), Grid]),
            {?BT_SUCCESS, BB, State#{path => Path, misc := Misc#{box_target_id => BoxId}}};
        Other ->
            rogue_adventure_mgr:game_over(error, io_lib:format("宝箱寻路报错了，没有做那么多容错，导致了这种结局，所以说，一定不能偷懒！！ Error:~w", [Other])),
            {?BT_SUCCESS, BB, State#{path => []}}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% 获取距离主角最近的一个宝箱位置
get_closest_box(BoxList, Pos) ->
    [{_, Id, ClosestGrid} | _] = lists:sort([{util:distance(Pos, Grid), Id, Grid} || {Id, Grid} <- BoxList]),

    %% 主角不能跟宝箱位置重合，所以只能挨着宝箱位置站
    [{_, Grid} | _] = lists:sort([
        {util:distance(Pos, Grid), Grid} || Grid <- util:grid_border(ClosestGrid, 1),
        not rogue_adventure_mgr:is_block(Grid)
    ]),
    {Id, Grid}.
