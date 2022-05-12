%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(unit_box).

-include("rogue_adventure.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

init(Args) ->
	State = do_init(Args),
	{ok, State}.

handle_call(run_frame, _From, State) ->
	{reply, ok, State};

handle_call({unlock_box, AtkId, AtkAttr}, _From, #{id := Id, pos := Pos, attr := #{hp := HP, def := Def, args := Prob} = Attr} = State) ->
	#{atk := TargetAtk, cri := TargetCri} = AtkAttr,
	HurtHP = unit:hurt_formula(TargetAtk, TargetCri, Def),
	case erlang:max(HP - HurtHP, 0) of
		0 ->
			case Prob >= util:rand(1, 100) of
				?true ->
					unlock_log(Id, AtkId, HurtHP, 0, <<"已解锁，获得了大量财宝"/utf8>>);
				?false ->
					unlock_log(Id, AtkId, HurtHP, 0, <<"已解锁，一个空箱子什么也没有"/utf8>>)
			end,
			do_spawn_monster(Pos),
			{stop, normal, already_unlock, State#{attr := Attr#{hp := 0}}};
		UpHP ->
			unlock_log(Id, AtkId, HurtHP, UpHP, <<"未解锁"/utf8>>),
			{reply, continue, State#{attr := Attr#{hp := UpHP}}}
	end;

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(run_frame, State) ->
	{noreply, State};

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

handle_continue(_Info, State) ->
	{noreply, State}.

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------
do_init(Args) ->
	UnitType = ?UNIT_TYPE_BOX,
	Id = rogue_adventure_mgr:get_id(),
	Pos = get_bron_pos(?WIDTH, ?HIGH),
	rogue_adventure_mgr:insert_unit_info(#ets_unit_info{id = Id, pid = self(), type = UnitType, pos = Pos}),
	rogue_adventure_mgr:update_maze_row(Pos, UnitType),
	#{
		id => Id,
		type => UnitType,
		pos => Pos,
		attr => unit:init_attr(UnitType, Args),
		misc => #{}
	}.

get_bron_pos(Width, High) ->
	Pos = {util:rand(1, Width), util:rand(1, High)},
	case rogue_adventure_mgr:is_block(Pos) of
		?true ->
			get_bron_pos(Width, High);
		?false ->
			Pos
	end.

unlock_log(Id, TargetId, HurtHP, HP, UnlockState) ->
	UnitType = rogue_adventure_mgr:get_unit_info_by_index(TargetId, #ets_unit_info.type),
	?DEBUG("~ts:受到`~ts` 造成伤害:~ts 当前血量:~ts 状态:~ts", [
		unit:unit_name(Id, ?UNIT_TYPE_BOX),
		unit:unit_name(TargetId, UnitType), color:red(erlang:integer_to_list(HurtHP)), color:green(erlang:integer_to_list(HP)), UnlockState
	]).

do_spawn_monster(Pos) ->
	UnitType = ?UNIT_TYPE_CENTIPEDE,
	#config_attr{args = Prob} = config_attr:get(UnitType),
	case Prob >= util:rand(1, 100) of
		?true ->
			case unit:get_born_pos_by_pos(Pos) of
				none ->
					ok;
				UpPos ->
					rogue_adventure_mgr:spawn_monster(UnitType, [{born, rand}, {pos, UpPos}])
			end;
		?false ->
			ok
	end.


