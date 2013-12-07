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
    handle_connect/3,
    handle_message/3,
    handle_error/3,
    handle_disconnect/3,
    terminate/1
]).

-define(STATE, ofs_handler_driver_state).
-record(?STATE, {
    handler_pid :: pid(),
    conn_handlers :: dict()
}).

% of_driver callbacks

init(IpAddr, DataPathId, Features, Version, Connection, Opt) ->
    % look up datapath id, reject if already there
    {ok, Pid} = ofs_handler_sup:find_handler(IpAddr, DataPathId, Features, Version, Connection, Opt),
    gen_server:call(Pid, {init, IpAddr, DataPathId, Features, Version, Connection, Opt}),
    {ok, store_conn_handler(Connection, Pid, #?STATE{handler_pid = Pid})}.

handle_connect(Connection, AuxId, State) ->
    Pid = State#?STATE.handler_pid,
    {ok, ConnPid} = gen_server:call(Pid, {connect, Connection, AuxId}),
    {ok, store_conn_handler(Connection, ConnPid, State)}.

handle_message(Connection, Msg, State) ->
    case gen_server:call(conn_handler(Connection, State),
                                                {message, Connection, Msg}) of
        ok ->
            {ok, State};
        {terminate, Reason} ->
            {terminate, Reason, erase_conn_handler(Connection, State)}
    end.

handle_error(Connection, Error, State) ->
    case gen_server:call(conn_handler(Connection, State),
                                                {error, Connection, Error}) of
        ok ->
            {ok, State};
        {terminate, Reason} ->
            {terminate, Reason, erase_conn_handler(Connection, State)}
    end.

handle_disconnect(Connection, Reason, State) ->
    ok = gen_server:call(conn_handler(Connection, State),
                                            {disconnect, Connection, Reason}),
    {ok, erase_conn_handler(Connection, State)}.

terminate(State) ->
    ok = gen_server:call(State#?STATE.handler_pid, terminate_from_driver).

% internal functions
conn_handler(Connection, State) ->
    dict:fetch(Connection, State#?STATE.conn_handlers).

store_conn_handler(Connection, ConnPid, State) ->
    State#?STATE{conn_handlers = dict:store(Connection, ConnPid,
                                                State#?STATE.conn_handlers)}.

erase_conn_handler(Connection, State) ->
    State#?STATE{conn_handlers = dict:erase(Connection,
                                                State#?STATE.conn_handlers)}.
