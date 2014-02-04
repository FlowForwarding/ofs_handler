%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc
%%% Default callback handler for ofs_handler callbacks.  Returns
%%% errors on connection attempts.
%%% @end
%%%-------------------------------------------------------------------
-module(ofs_handler_default_handler).
-copyright("2013, Erlang Solutions Ltd.").

% default callback handler for ofs_handler

-export([init/7,
         connect/8,
         disconnect/1,
         failover/2,
         handle_message/2,
         handle_error/2,
         terminate/1]).

% ofs_handler callbacks
init(_Handler, _IpAddr, _DatapathId, _Features, _Version, _Connection, _Opt) ->
    {error, no_handler}.

connect(_Handler, _IpAddr, _DatapathId, _Features, _Version, _Connection, _AuxId, _Opt) ->
    {error, no_handler}.

% default handler rejects connections
% these callbacks are never called

disconnect(_State) ->
    ok.

failover(_ActiveState, StandbyState) ->
    {ok, StandbyState}.

handle_message(_Message, _State) ->
    ok.

handle_error(_Reason, _State) ->
    ok.

terminate(_State) ->
    ok.
