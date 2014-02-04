%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc
%%% ofs_handler logic.
%%% @end
%%%-------------------------------------------------------------------
-module(ofs_handler_logic).
-copyright("2013, Erlang Solutions Ltd.").

-include("ofs_handler_logic.hrl").
-include_lib("ofs_handler/include/ofs_handler_logger.hrl").

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
        opt,
        subscriptions
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
    ofd_disconnect/4,
    ofd_terminate/4,
    call_active/2,
    get_connections/1
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
            {ok, Pid} = ofs_handler_logic_sup:start_child(DatapathId),
            Pid;
        [Handler = #handlers_table{}] ->
            Handler#handlers_table.handler_pid
    end,
    {ok, HandlerPid}.

ofd_init(HandlerPid, IpAddr, DatapathId, Features, Version, Connection, Opt) ->
    gen_server:call(HandlerPid,
            {init, IpAddr, DatapathId, Features, Version, Connection, Opt}).

ofd_connect(HandlerPid, IpAddr, DatapathId, Features, Version, Connection, AuxId, Opt) ->
    gen_server:call(HandlerPid,
            {connect, IpAddr, DatapathId, Features, Version, Connection, AuxId, Opt}).

ofd_message(MessagePid, Connection, Msg) ->
    gen_server:call(MessagePid, {message, Connection, Msg}).

ofd_error(MessagePid, Connection, Error) ->
    gen_server:call(MessagePid, {error, Connection, Error}).

ofd_disconnect(HandlerPid, MessagePid, Connection, Reason) ->
    gen_server:call(HandlerPid, {disconnect, MessagePid, Connection, Reason}).

ofd_terminate(HandlerPid, MessagePid, Connection, Reason) ->
    gen_server:call(HandlerPid, {terminate, MessagePid, Connection, Reason}).

%% ------------------------------------------------------------------
%% utility functions
%% ------------------------------------------------------------------

call_active(DatapathId, Command) ->
    case locate_active(DatapathId) of
        no_handler -> no_handler;
        HandlerPid -> gen_server:call(HandlerPid, Command)
    end.

get_connections(DatapathId) ->
    call_active(DatapathId, get_connections).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([DatapathId]) ->
    register_handler(DatapathId),
    Subscriptions = ets:new(subscriptions, [bag, protected]),
    {ok, #?STATE{datapath_id = DatapathId, subscriptions = Subscriptions}}.

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
handle_call({async_subscribe, Module, Item}, _From, State) ->
    Ret = subscribe(Module, Item, State),
    {reply, Ret, State};
handle_call({async_unsubscribe, Module, Item}, _From, State) ->
    Ret = unsubscribe(Module, Item, State),
    {reply, Ret, State};
handle_call({get_async_subscribe, Module}, _From, State) ->
    Ret = get_subscriptions(Module, State),
    {reply, Ret, State};
handle_call(terminate, _From, State) ->
    signal_stop(terminated_by_call),
    {reply, ok, State}; 
% handle callbacks from of_driver
handle_call({init, IpAddr, DatapathId, Features, Version, Connection, Opt},
                    _From, State = #?STATE{datapath_id = DatapathId,
                                           subscriptions = Subscriptions}) ->
    % switch connected to of_driver.
    % this is the main connection.
    CallbackModule = get_opt(callback_module, Opt),
    CallbackOpt = get_opt(callback_opt, Opt),
    State1 = State#?STATE{
        ipaddr = IpAddr,
        features = Features,
        of_version = Version,
        aux_connections = [],
        callback_mod = CallbackModule,
        opt = Opt
    },
    case do_callback(CallbackModule, init, [active, IpAddr, DatapathId,
                            Features, Version, Connection, CallbackOpt]) of
        {ok, CallbackState} ->
            {ok, MessagePid} = ofs_handler_message_sup:start_child(
                                    Connection, main, CallbackModule,
                                    CallbackState, Subscriptions),
            true = link(MessagePid),
            {reply, {ok, MessagePid},
                            State1#?STATE{main_connection = Connection,
                                          callback_state = CallbackState}};
        {error, Reason} ->
            {reply, {terminate, Reason}, State1}
    end;
handle_call({connect, IpAddr, DatapathId, Features,
                    Version, Connection, AuxId, Opt}, _From,
                    State = #?STATE{aux_connections = AuxConnections,
                                    subscriptions = Subscriptions}) ->
    % switch connected to of_driver.
    % this is an auxiliary connection.
    CallbackModule = get_opt(callback_module, Opt),
    CallbackOpt = get_opt(callback_opt, Opt),
    case do_callback(CallbackModule, connect, [active, IpAddr, DatapathId,
                        Features, Version, Connection, AuxId, CallbackOpt]) of
        {ok, CallbackState} ->
            {ok, MessagePid} = ofs_handler_message_sup:start_child(
                                    Connection, AuxId, CallbackModule,
                                    CallbackState, Subscriptions),
            true = link(MessagePid),
            State1 = State#?STATE{
                aux_connections = [{AuxId, Connection} | AuxConnections],
                callback_state = CallbackState},
            {reply, {ok, MessagePid}, State1};
        {error, Reason} ->
            {reply, {terminate, Reason}, State}
    end;
handle_call({disconnect, MessagePid, Connection, Reason}, _From, #?STATE{
                                aux_connections = AuxConnections} = State) ->
    % lost an auxiliary connection
    ok = ofs_handler_message:disconnect(MessagePid, Reason),
    NewAuxConnections = lists:keydelete(Connection, 2, AuxConnections),
    {reply, ok, State#?STATE{aux_connections = NewAuxConnections}};
handle_call({terminate, MessagePid, _Connection, Reason}, _From, State) ->
    % lost the main connection
    ok = ofs_handler_message:disconnect(MessagePid, Reason),
    {reply, ok, State#?STATE{main_connection = undefined}};
handle_call(get_connections, _From, State) ->
    % return connections
    MainConnection = State#?STATE.main_connection,
    AuxConnections = State#?STATE.aux_connections,
    {reply, {MainConnection, AuxConnections}, State};
handle_call(_Request, _From, State) ->
    % unknown request
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #?STATE{datapath_id = DatapathId,
                          callback_state = CallbackState,
                          callback_mod = Module}) ->
    ?WARNING("OFS HANDLER logic terminate Reason : ~p",[Reason]),
    unregister_handler(DatapathId),
    do_callback(Module, terminate, [CallbackState]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

locate_active(DatapathId) ->
    case ets:lookup(?HANDLERS_TABLE, DatapathId) of
        [] -> no_handler;
        [#handlers_table{handler_pid = HandlerPid}] -> HandlerPid
    end.

register_handler(DatapathId) ->
    true = ets:insert_new(?HANDLERS_TABLE, #handlers_table{
                                                datapath_id = DatapathId,
                                                handler_pid = self()}).

unregister_handler(DatapathId) ->
    true = ets:delete(?HANDLERS_TABLE, DatapathId).

signal_stop(Reason) ->
    ?INFO("ofs_handler(~p) stopping: ~p", [self(), Reason]),
    gen_server:cast(self(), stop).

do_callback(undefined,_, _) ->
    ok;
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

subscribe(Module, Type, State) when is_atom(Type) ->
    subscribe(Module, {Type, true}, State);
subscribe(Module, {Type, FilterFn}, #?STATE{subscriptions = Subscriptions}) ->
    true =ets:insert(Subscriptions, {Type, Module, FilterFn}),
    ok.

unsubscribe(Module, Type, State) when is_atom(Type) ->
    unsubscribe(Module, {Type, true}, State);
unsubscribe(Module, {Type, FilterFn}, #?STATE{subscriptions = Subscriptions}) ->
    true =ets:delete_object(Subscriptions, {Type, Module, FilterFn}),
    ok.

get_subscriptions(Module, #?STATE{subscriptions = Subscriptions}) ->
    ets:match_object(Subscriptions, {'_', Module, '_'}).
