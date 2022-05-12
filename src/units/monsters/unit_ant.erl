%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%% 火蚂
%%% @end
%%%-------------------------------------------------------------------
-module(unit_ant).

-include("rogue_adventure.hrl").

%% API
-export([do_init/2, do_attack_target/4, do_died/1]).
%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------
do_init(_Args, State) ->
	State.

do_attack_target(_AtkId, _AtkAttr, _HurtHP, State) ->
	State.

do_died(State) ->
	State.