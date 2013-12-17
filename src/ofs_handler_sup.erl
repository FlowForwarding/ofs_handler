%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Ruan Pienaar <ruan.pienaar@erlang-solutions.com>
%%% @doc 
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(ofs_handler_sup).
-copyright("2013, Erlang Solutions Ltd.").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([find_handler/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1000, 3600}, []}}.

find_handler(_DataPathId) ->
    % look up DataPathId to find ofs_handler_logic pid
    ok.
