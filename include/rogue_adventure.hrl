%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-ifndef(ROGUE_ADVENTURE_H).
-define(ROGUE_ADVENTURE_H, true).

-include("config_attr.hrl").

-type row() :: tuple().
-type grid() :: {X :: integer(), Y :: integer()}.

-define(ETS_MAZE_DATA, ets_maze_data).                                                  % 地图信息
-define(ETS_UNIT_INFO, ets_unit_info).                                                  % 单位信息
-define(ETS_UNIT_INFO_INDEX_BY_POS, ets_unit_info_index_by_pos).                        % 单位信息grid索引 {pos, id}
-define(ETS_UNIT_INFO_INDEX_BY_PID, ets_unit_info_index_by_pid).                        % 单位信息pid索引  {pid, id}
-define(FRAME_PER_SEC, 10).                                                             % 每秒帧数
-define(RENDER_MAZE_INTERVAL, (2000 div ?FRAME_PER_SEC)).                               % 地图渲染间隔
-define(RECOVER_HP_INTERVAL, 10000).                                                    % 血量恢复间隔

%% 大地图单位信息
-record(ets_unit_info, {
	id :: integer(),
	pid :: pid(),
	type :: unit_type(),
	pos :: grid()
}).

-define(true, true).
-define(false, false).
-define(undefined, undefined).

-define(WIDTH, persistent_term:get(width)).                                             % 地图宽度
-define(HIGH, persistent_term:get(high)).                                               % 地图高度
-define(START_TIME, persistent_term:get(start_time)).

-define(IF(Condition, T, F), (case Condition of true -> T; false -> F end)).
-define(DEBUG(Format, Args),
	begin
		__Str = io_lib:format(unicode:characters_to_list(["~-12ts", Format, "\n"]), [util:format_game_time() | Args]),
		gen_server:cast(persistent_term:get(live_pid), {msg, __Str})
	end).
-define(DEBUG(Format), ?DEBUG(Format, [])).


%% 单位类型
-type unit_type() :: 0 .. 7.
-define(UNIT_TYPE_WALL, 0).                                                             % 不可行走单位
-define(UNIT_TYPE_EMPTY, 1).                                                            % 可行走单位
-define(UNIT_TYPE_ROGUE, 2).                                                            % 罗格
-define(UNIT_TYPE_MAGGOT, 3).                                                           % 恶蛆
-define(UNIT_TYPE_CENTIPEDE, 4).                                                        % 蜈蚣
-define(UNIT_TYPE_SCORPION, 5).                                                         % 毒蝎
-define(UNIT_TYPE_ANT, 6).                                                              % 火蚁
-define(UNIT_TYPE_BOX, 7).                                                              % 宝箱

-define(IS_MONSTER(UnitType), ((120 band (1 bsl UnitType)) > 0)).                        % 二进制01111000

%% 单位状态
-type unit_state() :: 1 .. 8.
-define(UNIT_STATE_IDLE, 1).                                                            % 空闲状态
-define(UNIT_STATE_ATTACK, 2).                                                          % 攻击状态
-define(UNIT_STATE_MOVE, 3).                                                            % 移动状态
-define(UNIT_STATE_DODGE, 4).                                                           % 躲避状态
-define(UNIT_STATE_FOLLOW, 5).                                                          % 追击状态
-define(UNIT_STATE_SEARCH, 6).                                                          % 寻宝状态
-define(UNIT_STATE_UNLOCK, 7).                                                          % 解锁状态
-define(UNIT_STATE_RECOVER, 8).                                                         % 恢复状态
-define(UNIT_STATE_PATROL, 9).                                                          % 巡逻状态

-define(IS_STATE(StateType, UnitState), (StateType =:= UnitState)).                      % 判断单位状态

-type attr() :: #{
name := string(),                                                                   % 名称
hp := integer(),                                                                    % 血量
atk := integer(),                                                                   % 攻击力
def := integer(),                                                                   % 防御力
cri := integer(),                                                                   % 暴击率
spd := integer(),                                                                   % 攻击速度
mov := integer(),                                                                   % 移动速度
guard_range => integer(),                                                           % 警戒范围
atk_range => integer(),                                                             % 攻击范围
cd => integer()                                                                     % 攻击冷却时间戳
}.

-type misc() :: #{
box_list => list(),                                                                 % 宝箱列表
box_target_id => integer()                                                          % 目标宝箱id
}.

-type state() :: #{
id := integer(),                                                                    % 唯一id
bb := reference(),                                                               % 行为树Id
passivity_bb := reference(),                                                     % 被动行为树（罗格）
type := unit_type(),                                                                % 单位类型
state => unit_state(),                                                              % 当前状态
pos := grid(),                                                                      % 当前位置
path => list(),                                                                     % 移动坐标路径
target_id => integer(),                                                             % 攻击目标
attr => attr(),                                                                     % 属性
misc := misc()                                                                      % 杂项
}.

% 变为空闲状态后要删除的数据
-define(IDLE_DELETE_KEY_LIST, [path, target_id]).

%% 变为空闲状态后要清理的misc数据
-define(IDLE_ERASE_MISC_KEY_LIST, [atk_id, atk_attr, box_target_id, attack_result]).

-endif.