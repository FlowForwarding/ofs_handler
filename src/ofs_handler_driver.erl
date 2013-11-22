%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc 
%%% Handle callbacks from of_driver.
%%% @end
%%%-------------------------------------------------------------------
-module(ofs_handler).
-copyright("2013, Erlang Solutions Ltd.").

-type ip_address() :: string().
-type datapath_id() :: binary().
-type of_version() :: integer().
-type of_connection() :: term().
-type ofd_options() :: term().
-type ofs_error_reason() :: term().


% of_driver callbacks

init(IpAddr, DataPathId, Features, Version, Connection, Opt) ->
    % look up datapath id, reject if already there
    {ok, Pid} = ofs_handler_sup:start_handler(IpAddr, DataPathId, Features, Version, Connection, Opt),
    {ok, #ofs_handler_driver_state{handler_pid = Pid}}.

handle_connect(Connection, State) ->
    % look up pid by datapath id
    Pid = State#ofs_handler_driver_state.handler_pid,
    gen_server:call(Pid, {connect, Connection}),
    {ok, State}.

handle_message(Connection, Msg, State) ->
    % look up pid by datapath id
    Pid = State#ofs_handler_driver_state.handler_pid,
    gen_server:call(Pid, {message, Connection, Msg}),
    {ok, State}.

handle_error(Connection, Error, State) ->
    % look up pid by datapath id
    Pid = State#ofs_handler_driver_state.handler_pid,
    gen_server:call(Pid, {error, Connection, Error}),
    {ok, State}.

handle_disconnect(Connection, Reason, State) ->
    % look up pid by datapath id
    Pid = State#ofs_handler_driver_state.handler_pid,
    gen_server:call(Pid, {disconnect, Connection, Reason}),
    {ok, State}.

terminate(State) ->
    % look up pid by datapath id
    Pid = State#ofs_handler_driver_state.handler_pid,
    gen_server:call(Pid, terminate),
    {ok, State}.
