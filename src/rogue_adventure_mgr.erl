%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rogue_adventure_mgr).

-behaviour(gen_server).

-include("rogue_adventure.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([
    start_link/1,
    get_id/0,
    get_maze_row/1,
    update_maze_row/2,
    is_block/1,
    get_maze_grid/1,
    insert_unit_info/1,
    update_unit_info/2,
    get_unit_info/1,
    get_unit_info_by_pos/1,
    get_unit_info_by_index/2,
    has_unit_info/1,
    game_over/2,
    spawn_monster/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(CLEAR, <<"\e[H\e[J">>).

-record(state, {
    parent_pid,
    frame,
    timer_ref,
    is_change = ?false,
    rogue_tree_bb,
    monster_tree_bb
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% 获取唯一id
-spec get_id() -> integer().
get_id() ->
    atomics:add_get(persistent_term:get(counter_ref), 1, 1).

-spec get_maze_row(integer()) -> row().
get_maze_row(Y) ->
    ets:lookup_element(?ETS_MAZE_DATA, Y, 2).

-spec update_maze_row(grid(), unit_type()) -> ok.
update_maze_row(Grid, UnitType) ->
    do_update_maze_row(Grid, UnitType),
    gen_server:cast(?MODULE, update_maze).

%% 是否是障碍
-spec is_block(grid()) -> boolean().
is_block({X, Y}) ->
    element(X, get_maze_row(Y)) =/= ?UNIT_TYPE_EMPTY.

%% 获取格子单元类型
-spec get_maze_grid(grid()) -> unit_type().
get_maze_grid({X, Y}) ->
    element(X, get_maze_row(Y)).

%% 插入单元数据
-spec insert_unit_info(#ets_unit_info{}) -> ok.
insert_unit_info(UnitInfo) ->
    ?true = ets:insert_new(?ETS_UNIT_INFO, UnitInfo),
    ?true = ets:insert_new(?ETS_UNIT_INFO_INDEX_BY_POS, {
        UnitInfo#ets_unit_info.pos, UnitInfo#ets_unit_info.id
    }),
    ?true = ets:insert_new(?ETS_UNIT_INFO_INDEX_BY_PID, {
        UnitInfo#ets_unit_info.pid, UnitInfo#ets_unit_info.id
    }),
    ok.

%% 更新单元数据
-spec update_unit_info(integer(), list()) -> ok.
update_unit_info(Id, UpdateList) ->
    case lists:keyfind(#ets_unit_info.pos, 1, UpdateList) of
        ?false ->
            ets:update_element(?ETS_UNIT_INFO, Id, UpdateList);
        {_, UpPos} ->
            OldPos = get_unit_info_by_index(Id, #ets_unit_info.pos),
            ets:update_element(?ETS_UNIT_INFO, Id, UpdateList),
            ?true = ets:insert_new(?ETS_UNIT_INFO_INDEX_BY_POS, {UpPos, Id}),
            ets:delete(?ETS_UNIT_INFO_INDEX_BY_POS, OldPos)
    end.

%% 获取单位信息
-spec get_unit_info(integer()) -> #ets_unit_info{} | ?undefined.
get_unit_info(Id) ->
    case ets:lookup(?ETS_UNIT_INFO, Id) of
        [] ->
            ?undefined;
        [UnitInfo] ->
            UnitInfo
    end.

%% 获取单位信息根据位置坐标
-spec get_unit_info_by_pos(grid()) -> #ets_unit_info{} | ?undefined.
get_unit_info_by_pos(Pos) ->
    case ets:lookup(?ETS_UNIT_INFO_INDEX_BY_POS, Pos) of
        [] ->
            ?undefined;
        [{_, Id}] ->
            get_unit_info(Id)
    end.

%% 根据索引获取单位信息
-spec get_unit_info_by_index(integer(), pos_integer()) -> term() | ?undefined.
get_unit_info_by_index(Id, Index) ->
    try
        ets:lookup_element(?ETS_UNIT_INFO, Id, Index)
    catch
        _:_ ->
            ?undefined
    end.

%% 单位是否存在
-spec has_unit_info(integer() | grid()) -> boolean().
has_unit_info(Id) when is_integer(Id) ->
    ets:member(?ETS_UNIT_INFO, Id);
has_unit_info(Grid) when is_tuple(Grid) ->
    ets:member(?ETS_UNIT_INFO_INDEX_BY_POS, Grid).

%% 产生一个怪物
-spec spawn_monster(unit_type(), term()) -> any().
spawn_monster(UnitType, Args) ->
    gen_server:cast(?MODULE, {spawn_monster, UnitType, Args}).

%% 游戏结束
-spec game_over(atom(), string()) -> ok.
game_over(Result, Reason) ->
    gen_server:cast(?MODULE, {game_over, Result, Reason}).

init(Args) ->
    erlang:process_flag(trap_exit, ?true),

    persistent_term:put(width, proplists:get_value(width, Args, element(2, io:columns()) - 3)),
    persistent_term:put(high, proplists:get_value(high, Args, element(2, io:rows()) - 3)),

    ets:new(?ETS_MAZE_DATA, [public, set, named_table]),
    ets:new(?ETS_UNIT_INFO, [public, named_table, set, {keypos, #ets_unit_info.id}]),
    ets:new(?ETS_UNIT_INFO_INDEX_BY_PID, [public, named_table, set]),
    ets:new(?ETS_UNIT_INFO_INDEX_BY_POS, [public, named_table, set]),

    State = #state{parent_pid = proplists:get_value(parent_pid, Args, ?undefined)},
    UpState = do_init_behavior_tree(State),
    {ok, UpState}.

handle_call(
    {run, LivePid},
    _From,
    #state{timer_ref = TimerRef, rogue_tree_bb = RogueTreeBB, monster_tree_bb = MonsterTreeBB} =
        State
) ->
    persistent_term:put(start_time, util:now_msec()),
    persistent_term:put(live_pid, LivePid),
    init_counter(),
    do_create_maze(),
    do_gen_unit(RogueTreeBB, MonsterTreeBB),
    util:cancel_timer(TimerRef),
    {reply, ok, State#state{frame = 0, timer_ref = start_frame_timer()}};
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(update_maze, State) ->
    {noreply, State#state{is_change = ?true}};
handle_cast({game_over, Result, Reason}, #state{timer_ref = TimerRef} = State) ->
    ?DEBUG("Result:~w Reason:~ts", [Result, Reason]),
    util:cancel_timer(TimerRef),
    ets:delete_all_objects(?ETS_UNIT_INFO),
    ets:delete_all_objects(?ETS_UNIT_INFO_INDEX_BY_PID),
    ets:delete_all_objects(?ETS_UNIT_INFO_INDEX_BY_POS),
    gen_server:cast(persistent_term:get(live_pid), game_over),
    {noreply, State};
handle_cast({spawn_monster, UnitType, Args}, #state{monster_tree_bb = BB} = State) ->
    do_gen_unit(UnitType, 1, [{blackboard, BB} | Args]),
    {noreply, State};
handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info({timeout, _Ref, refresh_timeout}, #state{frame = Frame, is_change = IsChange} = State) ->
    %%	?DEBUG("Frame:~w", [Frame]),
    start_frame_timer(),
    do_run_frame(),
    ?IF(IsChange, do_render_maze(), ok),
    {noreply, State#state{frame = Frame + 1, is_change = ?false}};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{}) ->
    try
        [{Pid, Id}] = ets:take(?ETS_UNIT_INFO_INDEX_BY_PID, Pid),
        [#ets_unit_info{id = Id, type = UnitType, pos = Pos}] = ets:take(?ETS_UNIT_INFO, Id),
        ets:delete(?ETS_UNIT_INFO_INDEX_BY_POS, Pos),
        do_update_maze_row(Pos, ?UNIT_TYPE_EMPTY),
        ?DEBUG("~ts:从地图中消失了", [unit:unit_name(Id, UnitType)])
    catch
        _:_ ->
            ok
    end,
    {noreply, State};
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 唯一id生成器
init_counter() ->
    persistent_term:put(counter_ref, atomics:new(1, [])).

%% 创建迷宫
do_create_maze() ->
    MazeData = island_maze:create_maze(?WIDTH, ?HIGH),
    ets:insert(?ETS_MAZE_DATA, lists:zip(lists:seq(1, ?HIGH), erlang:tuple_to_list(MazeData))),
    ok.

%% 启动帧定时器
start_frame_timer() ->
    erlang:start_timer(?RENDER_MAZE_INTERVAL, self(), refresh_timeout).

do_init_behavior_tree(State) ->
    Path = code:priv_dir(rogue_adventure),
    {ok, MonTreeMod} = behavior_tree:load_tree_file(Path ++ "/behavior_tree/monster_ai.json"),
    MonBB = blackboard:init_blackboard(MonTreeMod, <<"怪物AI"/utf8>>),

    {ok, RogueTreeMod} = behavior_tree:load_tree_file(Path ++ "/behavior_tree/rogue_ai.json"),
    RogueBB = blackboard:init_blackboard(RogueTreeMod, <<"罗格AI"/utf8>>),
    RoguePassivityBB = blackboard:init_blackboard(RogueTreeMod, <<"罗格被动AI"/utf8>>),
    State#state{rogue_tree_bb = {RogueBB, RoguePassivityBB}, monster_tree_bb = MonBB}.

%% 生成地图单位
do_gen_unit(RogueTreeBB, MonsterTreeBB) ->
    Args = [{born, init}],
    do_gen_monster_unit(MonsterTreeBB, Args),
    do_gen_box_unit(Args),
    do_gen_rogue_unit(RogueTreeBB, Args).

do_gen_monster_unit(BB, Args) ->
    UpArgs = [{blackboard, BB} | Args],
    Fun = fun({UnitType, Count}) -> do_gen_unit(UnitType, Count, UpArgs) end,
    Num = erlang:trunc(?WIDTH * ?HIGH * 0.001),
    lists:foreach(Fun, [
        {?UNIT_TYPE_SCORPION, Num},
        {?UNIT_TYPE_ANT, Num},
        {?UNIT_TYPE_MAGGOT, Num div 2},
        {?UNIT_TYPE_CENTIPEDE, Num div 2}
    ]).

do_gen_box_unit(Args) ->
    Num = erlang:trunc(?WIDTH * ?HIGH * 0.001),
    do_gen_unit(?UNIT_TYPE_BOX, Num, Args).

do_gen_rogue_unit({BB, PassivityBB}, Args) ->
    UpArgs = [{blackboard, BB}, {passivity_blackboard, PassivityBB} | Args],
    do_gen_unit(?UNIT_TYPE_ROGUE, 1, UpArgs).

do_gen_unit(UnitType, Count, Args) when Count > 0 ->
    {ok, Pid} = rogue_adventure_unit_sup:start_child([{unit_type, UnitType} | Args]),
    erlang:monitor(process, Pid),
    do_gen_unit(UnitType, Count - 1, Args);
do_gen_unit(_UnitType, _Count, _Args) ->
    ok.

%% 更新迷宫格子
do_update_maze_row({X, Y}, Value) ->
    Row = get_maze_row(Y),
    ets:update_element(?ETS_MAZE_DATA, Y, {2, setelement(X, Row, Value)}).

%% 渲染迷宫
do_render_maze() ->
    Border = [lists:duplicate(?WIDTH + 2, convert(?UNIT_TYPE_WALL)), "\n"],
    TopBorder = Border,
    BottomBorder = Border,
    Str = io_lib:format(erlang:iolist_to_binary([?CLEAR, "~ts"]), [
        [TopBorder, row_border(1, ?HIGH), BottomBorder]
    ]),
    gen_server:cast(persistent_term:get(live_pid), {maze, Str}).

row_border(RowNum, High) when RowNum =< High ->
    Row = rogue_adventure_mgr:get_maze_row(RowNum),
    [
        [
            convert(?UNIT_TYPE_WALL),
            [convert(E) || E <- tuple_to_list(Row)],
            convert(?UNIT_TYPE_WALL),
            "\n"
        ]
        | row_border(RowNum + 1, High)
    ];
row_border(_RowNum, _High) ->
    [].

convert(?UNIT_TYPE_WALL) ->
    color:white("X");
convert(?UNIT_TYPE_EMPTY) ->
    color:black(" ");
convert(?UNIT_TYPE_ROGUE) ->
    #config_attr{sign = Sign} = config_attr:get(?UNIT_TYPE_ROGUE),
    ?IF(util:now_sec() rem 2 == 0, color:on_true("006400", Sign), color:on_true("859900", Sign));
convert(UnitType) ->
    convert_1(UnitType).

convert_1(UnitType) ->
    #config_attr{sign = Sign, sign_color = Color} = config_attr:get(UnitType),
    color:Color(Sign).

do_run_frame() ->
    ets:safe_fixtable(?ETS_UNIT_INFO_INDEX_BY_PID, ?true),
    try
        do_run_frame_1(ets:first(?ETS_UNIT_INFO_INDEX_BY_PID))
    after
        ets:safe_fixtable(?ETS_UNIT_INFO_INDEX_BY_PID, ?false)
    end.

do_run_frame_1(Pid) when is_pid(Pid) ->
    try
        unit:run_frame(Pid)
    catch
        _:_ ->
            ok
    end,
    do_run_frame_1(ets:next(?ETS_UNIT_INFO_INDEX_BY_PID, Pid));
do_run_frame_1('$end_of_table') ->
    ok.
