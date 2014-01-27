-module(ofs_handler_default_handler).

% default callback handler for ofs_handler

-export([init/7,
         connect/8,
         disconnect/1,
         failover/2,
         handle_message/2,
         terminate/1]).

% ofs_handler callbacks
init(_Handler, _IpAddr, _DatapathId, _Features, _Version, _Connection, _Opt) ->
    {error, no_handler}.

connect(_Handler, _IpAddr, _DatapathId, _Features, _Version, _Connection, _AuxId, _Opt) ->
    {error, no_handler}.

% default handler rejects connections
% these callbacks are never called

disconnect(State) ->
    {ok, State}.

failover(_ActiveState, StandbyState) ->
    {ok, StandbyState}.

handle_message(_Message, State) ->
    {ok, State}.

terminate(_State) ->
    ok.
