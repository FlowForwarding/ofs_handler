%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc
%%% ofs_handler top level supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(ofs_handler_sup).
-copyright("2013, Erlang Solutions Ltd.").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->    
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    {ok, {SupFlags, [supervisor(ofs_handler_logic_sup),
                     supervisor(ofs_handler_message_sup)]}
    }.

supervisor(M) ->
    Restart = permanent,
    Shutdown = 2000,
    {M, {M, start_link, []}, Restart, Shutdown, supervisor, [M]}.
