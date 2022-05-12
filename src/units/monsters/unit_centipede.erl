%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 蜈蚣
%%% @end
%%%-------------------------------------------------------------------
-module(unit_centipede).

-include("rogue_adventure.hrl").

%% API
-export([do_init/2, do_attack_target/4, do_died/1]).
%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------
do_init(Args, #{id := Id} = State) ->
	case proplists:get_value(born, Args) of
		rand ->
			?DEBUG("~ts:终于有人解开宝箱了。",[unit:unit_name(Id)]);
		_ ->
			ok
	end,
	State.

do_attack_target(_AtkId, _AtkAttr, _HurtHP, State) ->
	State.

do_died(State) ->
	State.