%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 恶蛆
%%% @end
%%%-------------------------------------------------------------------
-module(unit_maggot).

-include("rogue_adventure.hrl").

%% API
-export([do_init/2, do_attack_target/4, do_died/1]).
%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------
do_init(Args, #{id := Id} = State) ->
	case proplists:get_value(born, Args) of
		rand ->
			?DEBUG("~ts:无限分裂，我的子子孙孙", [unit:unit_name(Id)]);
		_ ->
			ok
	end,
	State.


do_attack_target(_AtkId, _AtkAttr, _HurtHP, State) ->
	State.

do_died(#{type := UnitType, pos := Pos, attr := #{max_hp := MaxHP, args := Prob}} = State) ->
	case Prob >= util:rand(1, 100) of
		?true ->
			case unit:get_born_pos_by_pos(Pos) of
				none ->
					ok;
				UpPos ->
					rogue_adventure_mgr:spawn_monster(UnitType, [{born, rand}, {pos, UpPos}, {hp, MaxHP div 2}])
			end;
		?false ->
			ok
	end,
	State.