%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, 4399
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rogue_adventure_unit_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_child/1]).

start_child(Args) ->
    supervisor:start_child(?MODULE, [Args]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 0, period => 1},
    ChildSpec = #{
        id => unit,
        start => {unit, start_link, []},
        restart => temporary,
        shutdown => 1000,
        type => worker,
        modules => [unit]
    },
    {ok, {SupFlags, [ChildSpec]}}.


