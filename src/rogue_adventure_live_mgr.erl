%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rogue_adventure_live_mgr).

-behaviour(gen_server).

-include("rogue_adventure.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-define(MSG_SIZE, 13).

-record(state, {
	io,
	maze = "",
	msg_list = [],
	is_refresh = ?false
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(_Args) ->
	erlang:process_flag(trap_exit, ?true),
	{ok, IO} = file:open("rogue_adventure.log", [write, {encoding, utf8}]),
	start_refresh_timer(),
	{ok, #state{io = IO}}.

handle_call(_Request, _From, State = #state{}) ->
	{reply, ok, State}.

handle_cast({msg, Msg}, #state{io = IO, msg_list = MsgList} = State) ->
	io:format(IO, "~ts", [Msg]),
	{noreply, State#state{msg_list = [Msg | MsgList], is_refresh = ?true}};

handle_cast({maze, Maze}, State) ->
	{noreply, State#state{maze = Maze, is_refresh = ?true}};

handle_cast(game_over, State) ->
	UpState = do_refresh(State),
	{stop, normal, UpState};

handle_cast(_Request, State = #state{}) ->
	{noreply, State}.

handle_info(refresh_timeout, State) ->
	start_refresh_timer(),
	UpState = do_refresh(State),
	{noreply, UpState};

handle_info(_Info, State = #state{}) ->
	{noreply, State}.

terminate(_Reason, _State = #state{io = IO}) ->
	file:close(IO),
	ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_refresh_timer() ->
	erlang:send_after(?RENDER_MAZE_INTERVAL * 2, self(), refresh_timeout).

do_refresh(#state{is_refresh = IsRefresh, maze = Maze, msg_list = MsgList} = State) ->
	case IsRefresh of
		?true ->
			do_print_maze(Maze),
			UpMsgList = do_print_msg_list(MsgList),
			State#state{msg_list = UpMsgList, is_refresh = ?false};
		?false ->
			State
	end.

do_print_maze(Maze) ->
	io:format("~ts", [Maze]).

do_print_msg_list(MsgList) ->
	do_print_msg_list_1(MsgList, 0).

do_print_msg_list_1([Msg | T], N) when N < ?MSG_SIZE ->
	MsgList = do_print_msg_list_1(T, N + 1),
	io:format("~ts", [Msg]),
	[Msg | MsgList];
do_print_msg_list_1(_MsgList, _N) ->
	[].
