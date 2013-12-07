-module(ofs_handler_logic).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(STATE, ofs_handler_logic_state).
-record(?STATE,
    {
        ipaddr,
        datapathid,
        features,
        version,
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

-export([start_link/6]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(IpAddr, DataPathId, Features, Version, Connection, Opt) ->
    gen_server:start_link(?MODULE, [IpAddr, DataPathId, Features, Version, Connection, Opt], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([IpAddr, DataPathId, Features, Version, Connection, Opt]) ->
    gen_server:cast(self(), {init, IpAddr, DataPathId, Features, Version, Connection, Opt}),
    {ok, #?STATE{}}.

% handle API
handle_call(get_features, _From, State) ->
    {reply, ok, State};
handle_call(get_config, _From, State) ->
    {reply, ok, State};
handle_call(get_description, _From, State) ->
    {reply, ok, State};
handle_call({set_config, _ConfigFlag, _MissSendLength}, _From, State) ->
    {reply, ok, State};
handle_call({send_packet, _Data, _PortNumber, _Actions}, _From, State) ->
    {reply, ok, State};
handle_call(delete_all_flows, _From, State) ->
    {reply, ok, State};
handle_call({modify_flows, _FlowMods}, _From, State) ->
    {reply, ok, State};
handle_call(deleted_all_groups, _From, State) ->
    {reply, ok, State};
handle_call({modify_groups, _GroupMods}, _From, State) ->
    {reply, ok, State};
handle_call({modify_ports, _PortMods}, _From, State) ->
    {reply, ok, State};
handle_call({modify_meters, _MeterMods}, _From, State) ->
    {reply, ok, State};
handle_call({get_flow_statistics, _TableId, _Matches, _Options}, _From, State) ->
    {reply, ok, State};
handle_call({get_aggregate_statistics, _TableId, _Matches, _Options}, _From, State) ->
    {reply, ok, State};
handle_call(get_table_statistics, _From, State) ->
    {reply, ok, State};
handle_call({get_port_statistics, _PortNumber}, _From, State) ->
    {reply, ok, State};
handle_call({get_queue_statistics, _PortNumber, _QueueId}, _From, State) ->
    {reply, ok, State};
handle_call({get_group_statistics, _GroupId}, _From, State) ->
    {reply, ok, State};
handle_call({get_meter_statistics, _MeterId}, _From, State) ->
    {reply, ok, State};
handle_call(get_table_features, _From, State) ->
    {reply, ok, State};
handle_call(get_auth_table_features, _From, State) ->
    {reply, ok, State};
handle_call({set_table_features, _TableFeatures}, _From, State) ->
    {reply, ok, State};
handle_call(get_port_descriptions, _From, State) ->
    {reply, ok, State};
handle_call(get_auth_port_descriptions, _From, State) ->
    {reply, ok, State};
handle_call(get_group_descriptions, _From, State) ->
    {reply, ok, State};
handle_call(get_auth_group_descriptions, _From, State) ->
    {reply, ok, State};
handle_call(get_group_features, _From, State) ->
    {reply, ok, State};
handle_call({get_meter_configuration, _MeterId}, _From, State) ->
    {reply, ok, State};
handle_call({get_auth_meter_configuration, _MeterId}, _From, State) ->
    {reply, ok, State};
handle_call(get_meter_features, _From, State) ->
    {reply, ok, State};
handle_call(get_flow_descriptions, _From, State) ->
    {reply, ok, State};
handle_call(get_auth_flow_descriptions, _From, State) ->
    {reply, ok, State};
handle_call({experimenter, _ExpId, _Type, _Data}, _From, State) ->
    {reply, ok, State};
handle_call(barrier, _From, State) ->
    {reply, ok, State};
handle_call({get_queue_configuration, _PortNumber}, _From, State) ->
    {reply, ok, State};
handle_call({get_queue_auth_configuration, _PortNumber}, _From, State) ->
    {reply, ok, State};
handle_call({set_role, _Role, _GenerationId}, _From, State) ->
    {reply, ok, State};
handle_call(get_async_configuration, _From, State) ->
    {reply, ok, State};
handle_call({set_async_configuration, _PacketInMask, _PortStatusMask, _FlowRemoveMask}, _From, State) ->
    {reply, ok, State};
handle_call(ping_switch, _From, State) ->
    {reply, ok, State};
handle_call({async_subscribe, _Module, _Items}, _From, State) ->
    {reply, ok, State};
handle_call({get_async_subscribe, _Module}, _From, State) ->
    {reply, ok, State};
handle_call(terminate, _From, State) ->
    {reply, ok, State};

% handle call backs form of_driver
handle_call({error, _Connection, _Reason}, _From, State) ->
    {reply, ok, State};
handle_call({message, _Connection, _Message}, _From, State) ->
    {reply, ok, State};
handle_call({connect, _AuxConnection}, _From, State) ->
    {reply, ok, State};
handle_call(terminate_from_driver, _From, State) ->
    {stop, terminated_from_driver, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% needs an initialization path that does not include a connection
% two states - connected and not_connected
handle_cast({init, _IpAddr, DataPathId, _Features, Version, Connection, Options}, State) ->
    % callback module from opts
    Module = get_opt(callback_module, Options),

    % controller peer from opts
    Peer = get_opt(peer, Options),

    % callback module options
    ModuleOpts = get_opt(callback_opts, Options),

    % find out if this controller is active or standby
    Mode = my_controller_mode(Peer),


    State1 = State#?STATE{
    },

    % do callback
    % do this first so higher level is informed if the active/standby
    % initialization fails for some reason
    case do_callback(Module, init, [Mode, DataPathId, Version, ModuleOpts]) of
        {ok, CallbackState} ->
            State2 = State1#?STATE{callback_state = CallbackState},
	    % tell the switch our controller mode (active -> master
	    % or standby -> slave)
            case tell_controller_mode(Connection, Mode, State2) of
                {error, Reason, State3} ->
                    {stop, Reason, State3};
                {ok, State3} ->
                    case tell_standby(generation_id, State3) of
                        ok ->
                            {noreply, State3};
                        {error, Reason} ->
                            {stop, Reason, State3}
                    end
            end
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #?STATE{
                                main_connection = MainConn,
                                callback_state = CallbackState,
                                callback_mod = Module}) ->
    % remove self from datapath id map to avoid getting disconnect callbacks
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

tell_controller_mode(Connection, active, State = #?STATE{
                                            generation_id = GenId,
                                            main_connection = Connection,
                                            version = Version}) ->
    Msg = ofs_msg_lib:set_role(Version, master, GenId),
    State1 = State#?STATE{generation_id = next_generation_id(GenId)},
    case of_driver:sync_send(Connection, Msg) of
        {error, Reason} ->
            {error, Reason, State1};
        {ok, _Reply} ->
            {ok, State1}
    end;
tell_controller_mode(_Connection, standby, _State) ->
    ok.

next_generation_id(GenId) ->
    GenId + 1.

get_opt(Key, Options) ->
    Default = case application:get_env(ofs_handler, Key) of
        {ok, V} -> V;
        undefined -> undefined
    end,
    proplsits:get_value(Key, Options, Default).

my_controller_mode(_Peer) ->
    active.

tell_standby(generation_id, State) ->
    {ok, State}.
