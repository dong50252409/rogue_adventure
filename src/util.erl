%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(util).

-include("rogue_adventure.hrl").

%% API
-export([rand/1, rand/2, now_sec/0, now_msec/0, grid_range/2, distance/2, get_direction/2, grid_border/2, valid_fun/0, format_game_time/0, cancel_timer/1]).

-export([check_border/1, check_border/3]).

%% 返回一个随机数,结果为:[1,N]
rand(0) -> 0;
rand(N) when N > 0 ->
	rand:uniform(N).

%% @doc 返回两个数之间的随机数
rand(Min, Min) ->
	Min;
rand(Min, Max) ->
	M = Min - 1,
	rand(Max - M) + M.

now_sec() ->
	erlang:system_time(second).

now_msec() ->
	erlang:system_time(millisecond).

%% 寻路验证函数
valid_fun() ->
	Width = ?WIDTH,
	High = ?HIGH,
	fun({X, Y} = Grid) ->
		X > 0 andalso X < Width andalso Y > 0 andalso Y < High andalso
			not rogue_adventure_mgr:is_block(Grid)
	end.

%% 生成指定范围内格子列表
grid_range(Grid, 0) ->
	[Grid];
grid_range({X, Y}, Range) ->

	Seq = lists:seq(-Range, Range),
	[
		{X + XOffset, Y + YOffset} || XOffset <- Seq, YOffset <- Seq,
		check_border({X + XOffset, Y + YOffset})
	].

%% 检查边界
check_border(Grid) ->
	Width = ?WIDTH, High = ?HIGH,
	check_border(Grid, Width, High).

check_border({X, Y}, MaxWidth, MaxHigh) when X > 0 andalso X =< MaxWidth andalso Y > 0 andalso Y =< MaxHigh ->
	?true;
check_border(_Grid, _MaxWidth, _MaxHigh) ->
	?false.

%% 生成指定范围的最外圈格子
grid_border(_Grid, 0) ->
	[];
grid_border({X, Y}, Range) ->
	X1 = erlang:min(X + Range, ?WIDTH),
	X2 = erlang:max(X - Range, 1),
	Y1 = erlang:min(Y + Range, ?HIGH),
	Y2 = erlang:max(Y - Range, 1),
	lists:usort([{X3, Y1} || X3 <- lists:seq(X2, X1), util:check_border({X3, Y1})]
	++ [{X3, Y2} || X3 <- lists:seq(X2, X1), util:check_border({X3, Y2})]
		++ [{X1, Y3} || Y3 <- lists:seq(Y2, Y1), util:check_border({X1, Y3})]
		++ [{X2, Y3} || Y3 <- lists:seq(Y2, Y1), util:check_border({X2, Y3})]).

%% 两点距离公式,曼哈顿公式
distance({X1, Y1}, {X2, Y2}) ->
	erlang:abs(X1 - X2) + erlang:abs(Y1 - Y2).

%% 获取A B 两点的方向
get_direction({X1, Y1}, {X2, Y2}) ->
	{uniformization(X1, X2), uniformization(Y1, Y2)}.

uniformization(N1, N2) ->
	if
		N1 - N2 =:= 0 -> 0;
		N1 - N2 > 0 -> 1;
		true -> -1
	end.

format_game_time() ->
	DiffMSec = util:now_msec() - ?START_TIME,
	Sec = DiffMSec rem 60000 / 1000,
	Min = DiffMSec div 60000,
	io_lib:format("~w分~w秒 ", [Min, Sec]).

cancel_timer(TimerRef) when is_reference(TimerRef) ->
	case erlang:cancel_timer(TimerRef) of
		?false ->
			receive
				{timeout, _Ref, _} ->
					0
			after 0 ->
				false
			end;
		RemainTime ->
			RemainTime
	end;
cancel_timer(_) ->
	ok.