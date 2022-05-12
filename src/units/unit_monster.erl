%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(unit_monster).

-include("rogue_adventure.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

init(Args) ->
    UnitType = proplists:get_value(unit_type, Args),
    State = do_init(UnitType, Args),
    UpState = dispatch(UnitType, do_init, [Args, State]),
    {ok, UpState}.

handle_call(run_frame, _From, #{bb := BB} = State) ->
    {_BTStatus, UpBB, UpState} = behavior_tree:execute(BB, State),
    {reply, ok, UpState#{bb := UpBB}};

handle_call({attack_target, AtkId, AtkAttr}, _From, #{type := UnitType} = State) ->
    {HurtHP, UpState1} = unit:do_be_hit(AtkId, AtkAttr, State),
    UpState2 = dispatch(UnitType, do_attack_target, [AtkId, AtkAttr, HurtHP, UpState1]),
    case UpState2 of
        #{attr := #{hp := 0}} ->
            UpState3 = dispatch(UnitType, do_died, [UpState2]),
            {stop, normal, died, UpState3};
        #{} ->
            {reply, alive, UpState2}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(run_frame, #{bb := BB} = State) ->
    {_BTStatus, UpBB, UpState} = behavior_tree:execute(BB, State),
    {noreply, UpState#{bb := UpBB}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(recover_hp, State) ->
    UpState = unit:do_recover_hp(State),
    {noreply, UpState};

handle_info(_Info, State) ->
    {noreply, State}.

handle_continue(_Info, State) ->
    {noreply, State}.

dispatch(UnitType, F, A) ->
    #config_attr{module = M} = config_attr:get(UnitType),
    erlang:apply(M, F, A).

do_init(UnitType, Args) ->
    Id = rogue_adventure_mgr:get_id(),
    Pos = proplists:get_value(pos, Args, gen_bron_grid(?WIDTH, ?HIGH)),
    rogue_adventure_mgr:insert_unit_info(#ets_unit_info{id = Id, pid = self(), type = UnitType, pos = Pos}),
    rogue_adventure_mgr:update_maze_row(Pos, UnitType),
    {ok, IO} = file:open(unicode:characters_to_binary([unit:unit_name(Id, UnitType), "_bt.log"]), [append, {encoding, utf8}]),
    #{
        id => Id,
        bb => blackboard:set_io(IO, proplists:get_value(blackboard, Args)),
        type => UnitType,
        state => ?UNIT_STATE_IDLE,
        pos => Pos,
        attr => unit:init_attr(UnitType, Args),
        misc => #{}
    }.

gen_bron_grid(Width, High) ->
    {Px, Py} = Pos = {util:rand(1, Width), util:rand(1, High)},
    Fun =
        fun({X, Y}) ->
            X > 0 andalso X < Width andalso Y > 0 andalso Y < High
                andalso not rogue_adventure_mgr:is_block({X, Y})
        end,
    case lists:all(Fun, [{Px + X, Py + Y} || X <- lists:seq(-2, 2), Y <- lists:seq(-2, 2)]) of
        ?true ->
            Pos;
        ?false ->
            gen_bron_grid(Width, High)
    end.
