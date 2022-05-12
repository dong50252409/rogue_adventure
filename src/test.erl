%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test).

%% API
-export([run/0, stop/0, live/0]).

run() ->
	erlang:spawn(fun() -> rogue_adventure:main(["--width", "100", "--high", "35"]) end).

stop() ->
	application:stop(rogue_adventure).

live() ->
	{ok, Pid} = rogue_adventure_live_mgr:start_link([]),
	Node = 'main@127.0.0.1',
	gen_server:call({rogue_adventure_mgr, Node}, {run, Pid}).
