%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc 
%%% Handle callbacks from of_driver.
%%% @end
%%%-------------------------------------------------------------------
-module(ofs_handler_driver).
-copyright("2013, Erlang Solutions Ltd.").

% -type ip_address() :: string().
% -type datapath_id() :: binary().
% -type of_version() :: integer().
% -type of_connection() :: term().
% -type ofd_options() :: term().
% -type ofs_error_reason() :: term().

-export([
    init/6,
    handle_connect/7,
    handle_message/2,
    handle_error/2,
    handle_disconnect/2,
    terminate/2
]).

-define(STATE, ofs_handler_driver_state).
-record(?STATE, {
    handler_pid :: pid(),
    connection
    }).

% of_driver callbacks
init(IpAddr, DataPathId, Features, Version, Connection, Opt) ->
    {ok, Pid} = ofs_handler_logic:ofd_find_handler(DataPathId),
    {ok, ConnPid} = ofs_handler_logic:ofd_init(Pid,
                    IpAddr, DataPathId, Features, Version, Connection, Opt),
    {ok, #?STATE{handler_pid = ConnPid, connection = Connection}}.

handle_connect(IpAddr, DataPathId, Features, Version, Connection, AuxId, Opt) ->
    {ok, Pid} = ofs_handler_logic:ofd_find_handler(DataPathId),
    {ok, ConnPid} = ofs_handler_logic:ofd_connect(Pid,
                IpAddr, DataPathId, Features, Version, Connection, AuxId, Opt),
    {ok, #?STATE{handler_pid = ConnPid, connection = Connection}}.

handle_message(Msg, State = #?STATE{
                                handler_pid = ConnPid,
                                connection = Connection}) ->
    case ofs_handler_logic:ofd_message(ConnPid, Connection, Msg) of
        ok ->
            {ok, State};
        {terminate, Reason} ->
            {terminate, Reason, State}
    end.

handle_error(Error, State = #?STATE{
                                handler_pid = ConnPid,
                                connection = Connection}) ->
    case ofs_handler_logic:ofd_error(ConnPid, Connection, Error) of
        ok ->
            {ok, State};
        {terminate, Reason} ->
            {terminate, Reason, State}
    end.

handle_disconnect(Reason, #?STATE{
                                handler_pid = ConnPid,
                                connection = Connection}) ->
    ok = ofs_handler_logic:ofd_disconnect(ConnPid, Connection, Reason).

terminate(Reason, #?STATE{
                                handler_pid = ConnPid,
                                connection = Connection}) ->
    ok = ofs_handler_logic:ofd_terminate(ConnPid, Connection, Reason).