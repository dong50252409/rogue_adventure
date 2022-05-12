%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(unit_rogue).

-include("rogue_adventure.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

init(Args) ->
    State = do_init(Args),
    {ok, State}.

handle_call(run_frame, _From, #{bb := BB} = State) ->
    {_BTStatus, UpBB, UpState} = behavior_tree:execute(BB, State),
    {reply, ok, UpState#{bb := UpBB}};

handle_call({attack_target, AtkId, AtkAttr}, _From, #{passivity_bb := BB, misc := Misc} = State) ->
    {_Hurt, UpState} = unit:do_be_hit(AtkId, AtkAttr, State),
    UpState1 = UpState#{misc := Misc#{atk_id => AtkId}},
    {_BTStatus, UpBB, UpState2} = behavior_tree:execute(BB, UpState1),
    case UpState2 of
        #{attr := #{hp := 0}} ->
            {stop, normal, died, UpState2#{passivity_bb := UpBB}};
        #{} ->
            {reply, alive, UpState2#{passivity_bb := UpBB}}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(run_frame, #{bb := BB} = State) ->
    {_BTStatus, UpBB, UpState} = behavior_tree:execute(BB, State),
    {noreply, UpState#{bb := UpBB}};

handle_cast({back_hurt, TargetId, HurtHP}, #{passivity_bb := BB, misc := Misc} = State) ->
    UpState = do_back_hurt(TargetId, HurtHP, State),
    UpState1 = UpState#{misc := Misc#{atk_id => TargetId}},
    {_BTStatus, UpBB, UpState2} = behavior_tree:execute(BB, UpState1),
    case UpState2 of
        #{attr := #{hp := 0}} ->
            {stop, normal, UpState2#{bb := UpBB}};
        #{} ->
            {noreply, UpState2#{bb := UpBB}}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(recover_hp, State) ->
    UpState = unit:do_recover_hp(State),
    {noreply, UpState};

handle_info(_Info, State) ->
    {noreply, State}.

handle_continue(_Info, State) ->
    {noreply, State}.

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------
do_init(Args) ->
    UnitType = ?UNIT_TYPE_ROGUE,
    Id = rogue_adventure_mgr:get_id(),
    Pos = get_bron_pos(?WIDTH, ?HIGH),
    rogue_adventure_mgr:insert_unit_info(#ets_unit_info{id = Id, pid = self(), type = UnitType, pos = Pos}),
    rogue_adventure_mgr:update_maze_row(Pos, UnitType),
    {ok, IO} = file:open(unicode:characters_to_binary([unit:unit_name(Id, UnitType), "_bt.log"]), [append, {encoding, utf8}]),
    {ok, PassivityIO} = file:open(unicode:characters_to_binary([unit:unit_name(Id, UnitType), "_passivity_bt.log"]), [append, {encoding, utf8}]),

    #{
        id => Id,
        bb => blackboard:set_io(IO, proplists:get_value(blackboard, Args)),
        passivity_bb => blackboard:set_io(PassivityIO, proplists:get_value(passivity_blackboard, Args)),
        type => UnitType,
        state => ?UNIT_STATE_IDLE,
        pos => Pos,
        attr => unit:init_attr(UnitType, Args),
        misc => #{box_list => get_box_list()}
    }.

get_bron_pos(Width, High) ->
    Pos = {util:rand(1, Width), util:rand(1, High)},
    case rogue_adventure_mgr:is_block(Pos) of
        ?true ->
            get_bron_pos(Width, High);
        ?false ->
            Pos
    end.

get_box_list() ->
    Fun = fun
              (#ets_unit_info{id = Id, pos = Grid, type = ?UNIT_TYPE_BOX}, Acc) ->
                  [{Id, Grid} | Acc];
              (_, Acc) ->
                  Acc
          end,
    ets:foldl(Fun, [], ?ETS_UNIT_INFO).

do_back_hurt(AtkId, HurtHP, #{id := Id, type := UnitType, attr := #{max_hp := MaxHP, hp := HP} = Attr} = State) ->
    CurHP = erlang:max(HP - HurtHP, 0),
    UpState = State#{attr := Attr#{hp := CurHP}},
    ?DEBUG("~ts:受到`~ts`反弹攻击 造成伤害:~ts 当前血量:~ts 状态:~ts", [
        unit:unit_name(Id, UnitType), unit:unit_name(AtkId), color:red(erlang:integer_to_list(HurtHP)), color:green(erlang:integer_to_list(CurHP)), unit:get_health_state(MaxHP, CurHP)]),
    UpState.