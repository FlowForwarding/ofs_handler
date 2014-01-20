-module(ofs_handler_logic).

-include("ofs_handler_logic.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(STATE, ofs_handler_logic_state).
-record(?STATE,
    {
        ipaddr,
        datapath_id,
        features,
        of_version,
        main_connection,
        aux_connections,
        callback_mod,
        callback_state,
        generation_id,
        opt
    }
).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([
    ofd_find_handler/1,
    ofd_init/7,
    ofd_connect/8,
    ofd_message/3,
    ofd_error/3,
    ofd_disconnect/3,
    ofd_terminate/3
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(DatapathId) ->
    gen_server:start_link(?MODULE, [DatapathId], []).

ofd_find_handler(DatapathId) ->
    HandlerPid = case ets:lookup(?HANDLERS_TABLE, DatapathId) of
        [] ->
            % spin up new ofs_handler
            {ok, Pid} = ofs_handler_logic_sup:start_child(DatapathId),
            % XXX put code to remove the entry in ets somewhere.
            true = ets:insert_new(?HANDLERS_TABLE, #handlers_table{
                                                    datapath_id = DatapathId,
                                                    handler_pid = Pid}),
            Pid;
        [Handler = #handlers_table{}] ->
            Handler#handlers_table.handler_pid
    end,
    {ok, HandlerPid}.

ofd_init(Pid, IpAddr, DatapathId, Features, Version, Connection, Opt) ->
    gen_server:call(Pid,
            {init, IpAddr, DatapathId, Features, Version, Connection, Opt}).

ofd_connect(Pid, IpAddr, DatapathId, Features, Version, Connection, AuxId, Opt) ->
    gen_server:call(Pid,
            {connect, IpAddr, DatapathId, Features, Version, Connection, AuxId, Opt}).

ofd_message(Pid, Connection, Msg) ->
    gen_server:call(Pid, {message, Connection, Msg}).

ofd_error(Pid, Connection, Error) ->
    gen_server:call(Pid, {error, Connection, Error}).

ofd_disconnect(Pid, Connection, Reason) ->
    gen_server:call(Pid, {disconnect, Connection, Reason}).

ofd_terminate(Pid, Connection, Reason) ->
    gen_server:call(Pid, {terminate_from_driver, Connection, Reason}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([DatapathId]) ->
    {ok, #?STATE{datapath_id = DatapathId}}.

% handle API
handle_call({send, Msg}, _From, State) ->
    Ret = send(Msg, State),
    {reply, Ret, State};
handle_call({sync_send, Msg}, _From, State) ->
    Ret = sync_send(Msg, State),
    {reply, Ret, State};
handle_call({send_list, Msg}, _From, State) ->
    Ret = send_list(Msg, State),
    {reply, Ret, State};
handle_call({sync_send_list, Msg}, _From, State) ->
    Ret = sync_send_list(Msg, State),
    {reply, Ret, State};
handle_call({ping_switch, Timeout}, _From, State) ->
    Ret = ping(Timeout, State),
    {reply, Ret, State};
handle_call({async_subscribe, Module, Item}, _From, State) ->
    Ret = subscribe(Module, Item, State),
    {reply, Ret, State};
handle_call({get_async_subscribe, Module}, _From, State) ->
    Ret = get_subscriptions(Module, State),
    {reply, Ret, State};
handle_call(terminate, _From, State) ->
    {stop, terminated_by_call, ok, State}; 
% handle callbacks from of_driver
handle_call({init, IpAddr, DatapathId, Features, Version, Connection, Opt}, _From, State = #?STATE{datapath_id = DatapathId}) ->
    % switch connected to of_driver.
    % this is the main connection.
    CallbackModule = get_opt(callback_module, Opt),
    State1 = State#?STATE{
        ipaddr = IpAddr,
        features = Features,
        of_version = Version,
        main_connection = Connection,
        aux_connections = [],
        callback_mod = CallbackModule,
        opt = Opt
    },
    CallbackOpt = get_opt(callback_opt, Opt),
    case do_callback(CallbackModule, init, [active, IpAddr, DatapathId, Features, Version, Connection, CallbackOpt]) of
        {ok, CallbackState} ->
            {reply, {ok, self()},
                            State1#?STATE{callback_state = CallbackState}};
        {error, _Reason} ->
            {stop, normal, State1}
    end;
handle_call({connect, _IpAddr, _DatapathId, _Features, _Version, Connection, AuxId, _Opt}, _From, State = #?STATE{aux_connections = AuxConnections}) ->
    State1 = State#?STATE{aux_connections = [{AuxId, Connection} | AuxConnections]},
    % switch connected to of_driver.
    % this is an auxiliary connection.
    % XXX spawn connection handler
    {reply, {ok, self()}, State1};
handle_call({message, _Connection, _Message}, _From, State) ->
    % switch sent us a message
    % XXX lookup message type in ets
    % XXX process message
    {reply, ok, State};
handle_call({error, _Connection, _Reason}, _From, State) ->
    % error on the connection
    {reply, ok, State};
handle_call({disconnect, _Connection, _Reason}, _From, State) ->
    % lost an auxiliary connection
    {reply, ok, State};
handle_call({terminate_from_driver, _Connection, _Reason}, _From, State) ->
    % lost the main connection
    {stop, terminated_from_driver, ok, State};
handle_call(_Request, _From, State) ->
    % unknown request
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #?STATE{main_connection = MainConn,
                           callback_state = CallbackState,
                           callback_mod = Module}) ->
    % XXX remove self from datapath id map to avoid getting disconnect callbacks
    of_driver:close_connection(MainConn),
    do_callback(Module, terminate, [CallbackState]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_callback(Module, Function, Args) ->
    erlang:apply(Module, Function, Args).

get_opt(Key, Options) ->
    Default = case application:get_env(ofs_handler, Key) of
        {ok, V} -> V;
        undefined -> undefined
    end,
    proplists:get_value(Key, Options, Default).

% XXX process commands through ofs_store
send(Msg, State) ->
    Conn = State#?STATE.main_connection,
    of_driver:send(Conn, Msg).

sync_send(Msg, State) ->
    Conn = State#?STATE.main_connection,
    of_driver:sync_send(Conn, Msg).

send_list(Msgs, State) ->
    Conn = State#?STATE.main_connection,
    of_driver:send_list(Conn, Msgs).

sync_send_list(Msgs, State) ->
    Conn = State#?STATE.main_connection,
    of_driver:sync_send_list(Conn, Msgs).

ping(_Timeout, _State) ->
    ok.

subscribe(Module, in_packet, State) ->
    subscribe(Module, {in_packet, fun always/1}, State);
subscribe(_Module, {in_packet, _FilterFn}, _State) ->
    % XXX store subscription in ets?
    ok;
subscribe(_Module, _Item, _State) ->
    ok.

get_subscriptions(_Module, _State) ->
    ok.

always(_) -> true.
