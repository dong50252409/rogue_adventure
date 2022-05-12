%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 毒蝎
%%% @end
%%%-------------------------------------------------------------------
-module(unit_scorpion).

-include("rogue_adventure.hrl").

%% API
-export([do_init/2, do_attack_target/4, do_died/1]).
%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------
do_init(_Args, State) ->
	State.

do_attack_target(TargetId, _AtkAttr, HurtHP, #{id := Id, attr := #{args := Prob}} = State) ->
	case Prob >= util:rand(1, 100) of
		?true ->
			unit:back_hurt(TargetId, Id, erlang:trunc(HurtHP * 0.1)),
			State;
		?false ->
			State
	end.

do_died(State) ->
	State.