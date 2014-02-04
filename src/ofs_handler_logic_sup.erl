%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Ruan Pienaar <ruan.pienaar@erlang-solutions.com>
%%% @doc 
%%% ofs_handler_logic supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(ofs_handler_logic_sup).
-copyright("2013, Erlang Solutions Ltd.").

-include("ofs_handler_logic.hrl").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_child/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ?HANDLERS_TABLE = ets:new(?HANDLERS_TABLE, [named_table, public, set, {keypos, 2}]),
    C = ofs_handler_logic,
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags,
            [{C, {C, start_link, []}, temporary, 1000, worker, [C]}]}}.

start_child(DataPathId) ->
    supervisor:start_child(?MODULE, [DataPathId]).
