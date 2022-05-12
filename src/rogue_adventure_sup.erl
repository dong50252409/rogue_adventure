%%%-------------------------------------------------------------------
%%% @author GZ1417
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rogue_adventure_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_child/2]).

start_child(ModName, Args) ->
    supervisor:start_child(?MODULE, child_spec(ModName, [Args], worker, 2000)).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => rest_for_one, intensity => 0, period => 1},
    ChildList = [child_spec(rogue_adventure_unit_sup, [], supervisor, infinity)],
    {ok, {SupFlags, ChildList}}.


child_spec(ModName, Args, Type, Shutdown) ->
    #{
        id => ModName,
        start => {ModName, start_link, Args},
        restart => permanent,
        shutdown => Shutdown,
        type => Type,
        modules => [ModName]
    }.
