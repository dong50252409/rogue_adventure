%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(unit).

-behaviour(gen_server).

-include("rogue_adventure.hrl").

-export([start_link/1, run_frame/1, attack_target/3, unlock_box/3, back_hurt/3, get_born_pos_by_pos/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, terminate/2]).

-export([unit_name/1, unit_name/2, hurt_formula/3, init_attr/2, do_be_hit/3, do_recover_hp/1, start_recover_hp_timer/0, get_health_state/2]).
%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

run_frame(Pid) ->
	gen_server:call(Pid, run_frame).

%% 攻击目标
-spec attack_target(TargetId :: integer(), AtkId :: integer(), AtkAttr :: attr()) -> died|alive.
attack_target(TargetId, AtkId, AtkAttr) ->
	case rogue_adventure_mgr:get_unit_info_by_index(TargetId, #ets_unit_info.pid) of
		?undefined ->
			die;
		Pid ->
			gen_server:call(Pid, {attack_target, AtkId, AtkAttr})
	end.

%% 反射伤害
-spec back_hurt(TargetId :: integer(), AtkId :: integer(), HurtHP :: integer()) -> ok.
back_hurt(TargetId, AtkId, HurtHP) ->
	case rogue_adventure_mgr:get_unit_info_by_index(TargetId, #ets_unit_info.pid) of
		?undefined ->
			ok;
		Pid ->
			gen_server:cast(Pid, {back_hurt, AtkId, HurtHP})
	end.

-spec unlock_box(BoxId :: integer(), AtkId :: integer(), AtkAttr :: attr()) -> already_unlock|continue.
unlock_box(BoxId, AtkId, AtkAttr) ->
	case rogue_adventure_mgr:get_unit_info_by_index(BoxId, #ets_unit_info.pid) of
		?undefined ->
			already_unlock;
		Pid ->
			gen_server:call(Pid, {unlock_box, AtkId, AtkAttr})
	end.

init(Args) ->
	{ok, #{id := Id} = State} = init_unit(Args),
	?DEBUG("~ts:已初始化！", [unit_name(Id)]),
	{ok, State}.

handle_call(Request, From, #{type := UnitType} = State) ->
	dispatch(UnitType, handle_call, [Request, From, State]).

handle_cast(Request, #{type := UnitType} = State) ->
	dispatch(UnitType, handle_cast, [Request, State]).

handle_info(Info, #{type := UnitType} = State) ->
	dispatch(UnitType, handle_info, [Info, State]).

handle_continue(Info, #{type := UnitType} = State) ->
	dispatch(UnitType, handle_continue, [Info, State]).

terminate(_Reason, _State) ->
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

unit_name(Id) ->
	UnitType = rogue_adventure_mgr:get_unit_info_by_index(Id, #ets_unit_info.type),
	unit_name(Id, UnitType).
unit_name(Id, UnitType) ->
	case config_attr:get(UnitType) of
		#config_attr{name = Name, word_color = Color} ->
			color:Color(io_lib:format("~ts_~w", [Name, Id]));
		_ ->
			""
	end.

init_unit(Args) ->
	UnitType = proplists:get_value(unit_type, Args),
	Reply = dispatch(UnitType, init, [Args]),
	start_recover_hp_timer(),
	Reply.


dispatch(UnitType, F, A) when ?IS_MONSTER(UnitType) ->
	erlang:apply(unit_monster, F, A);
dispatch(UnitType, F, A) ->
	#config_attr{module = Module} = config_attr:get(UnitType),
	erlang:apply(Module, F, A).

%% 初始化单元属性
init_attr(UnitType, InitArgs) ->
	#config_attr{
		name = Name,
		atk = Atk, def = Def, mov = Mov, spd = Spd,
		hp = HP, cri = Cri, guard_range = GuardRange,
		atk_range = AtkRange, args = Args
	} = config_attr:get(UnitType),
	#{
		name => Name,
		atk => Atk, def => Def,
		mov => Mov, spd => Spd,
		max_hp => proplists:get_value(hp, InitArgs, HP), hp => proplists:get_value(hp, InitArgs, HP),
		cri => Cri, cd => 0,
		guard_range => GuardRange, atk_range => AtkRange,
		args => Args
	}.

%% 启动定时恢复血量定时器
start_recover_hp_timer() ->
	erlang:send_after(?RECOVER_HP_INTERVAL, self(), recover_hp).

%% 伤害公式
hurt_formula(Atk, Cri, TargetDef) ->
	erlang:max(erlang:trunc(Atk * 1.5 + Atk * util:rand(0, Cri) / 100 - TargetDef), 1).

%% 受到攻击
do_be_hit(AtkId, TargetAttr, State) ->
	#{
		id := Id, type := UnitType,
		attr := #{max_hp := MaxHP, hp := HP, def := Def} = Attr
	} = State,
	#{atk := TargetAtk, cri := TargetCri} = TargetAttr,

	HurtHP = unit:hurt_formula(TargetAtk, TargetCri, Def),
	UpHurtHP = erlang:max(HP - HurtHP, 0),
	AtkUnitType = rogue_adventure_mgr:get_unit_info_by_index(AtkId, #ets_unit_info.type),
	?DEBUG("~ts:受到`~ts`攻击 造成伤害:~ts 当前血量:~ts 状态:~ts", [
		unit_name(Id, UnitType), unit_name(AtkId, AtkUnitType), color:red(erlang:integer_to_list(HurtHP)), color:green(erlang:integer_to_list(UpHurtHP)), get_health_state(MaxHP, UpHurtHP)]),
	{UpHurtHP, State#{attr := Attr#{hp := UpHurtHP}}}.

%% 每隔一段时间恢复血量
do_recover_hp(#{id := Id, type := UnitType, attr := Attr} = State) ->
	start_recover_hp_timer(),
	case Attr of
		#{max_hp := MaxHP, hp := MaxHP} ->
			State;
		#{def := Def, max_hp := MaxHP, hp := HP} ->
			RecoverHP = erlang:trunc(Def * 10 / 100),
			UpHP = erlang:min(HP + RecoverHP, MaxHP),
			?DEBUG("~ts:恢复血量:~ts 当前血量:~ts 状态:~ts", [unit_name(Id, UnitType), color:greenb(erlang:integer_to_list(RecoverHP)), color:green(erlang:integer_to_list(UpHP)), get_health_state(MaxHP, UpHP)]),
			State#{attr := Attr#{hp := UpHP}}
	end.

get_health_state(MaxHP, HP) when HP * 100 div MaxHP >= 30 ->
	<<"健康"/utf8>>;
get_health_state(MaxHP, HP) when HP > 0 andalso HP * 100 div MaxHP < 30 ->
	<<"濒死"/utf8>>;
get_health_state(_MaxHP, _HP) ->
	<<"已死亡"/utf8>>.

%% 根据位置随机周围位置
get_born_pos_by_pos(Pos) ->
	L = [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}],
	get_born_pos_by_pos_1(Pos, L).

get_born_pos_by_pos_1({X, Y}, L) ->
	{XOffset, YOffset} = lists:nth(util:rand(1, erlang:length(L)), L),
	UpL = lists:delete({XOffset, YOffset}, L),
	UpPos = {X + XOffset, Y + YOffset},
	?IF(not rogue_adventure_mgr:is_block(UpPos) andalso not rogue_adventure_mgr:has_unit_info(UpPos), UpPos, get_born_pos_by_pos_1({X, Y}, UpL));
get_born_pos_by_pos_1(_Grid, []) ->
	none.