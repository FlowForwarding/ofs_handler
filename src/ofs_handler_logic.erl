-module(ofs_handler_logic).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(ofsh_logic_state,
    {
        ipaddr,
        datapath_id,
        features,
        version,
        main_connection,
        aux_connections,
        callback_mod,
        callback_state,
        opt
    }
).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(IpAddr, DataPathId, Features, Version, Connection, Opt) ->
    gen_server:start_link(?MODULE, [IpAddr, DAtaPathId, Features, Version, Connection, Opt], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([IpAddr, DataPathId, Features, Version, Connection, Opt]) ->
    gen_server:cast(self(), {init, IpAddr, DataPathId, Features, Version, Connection, Opt}),
    {ok, #ofsh_logic_state{}}.

handle_call({error, Connection, Reason}, _From, State) ->
    {reply, ok, State};
handle_call({message, Connection, Message}, _From, State) ->
    {reply, ok, State};
handle_call({connect, AuxConnection}, _From, State) ->
    {reply, ok, State};
handle_call(terminate, _From, State) ->
    {stop, terminated_by_request, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({init, IpAddr, DataPathId, Features, Version, Connection, Opt}, State) ->
    % callback module from opts
    Module =

    % controller peer from opts
    Peer = 

    % find out if this controller is active or standby
    Mode = my_controller_mode(Peer),

    State1 = State#ofsh_logic_state{
        ...
    },

    % do callback
    % do this first so higher level is informed if the active/standby
    % initialization fails for some reason
    case do_callback(Module, init, [Mode, DataPathId, Version, Opt]) of
        {ok, CallbackState} ->
            State3 = State2#ofsh_logic_state{callback_state = CallbackState},
            {noreply, State3};
        {error, Reason} ->
            {stop, Reason, State2}
    end,

    % tell the switch our controller mode (active -> master or standby -> slave)
    case tell_controller_mode(Connection, Mode, State1) of
        {error, Reason, State2} ->
            % terminate
        {ok, State2} ->
    tell_standby(generation_id, State2),

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #ofsh_logic_state{main_connection = MainConn} = State) ->
    % remove self from datapath id map to avoid getting disconnect callbacks
    of_driver:close_connection(MainConn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_callback(Module, Function, Args) ->
    erlang:apply(Module, Function, Args).

tell_controller_mode(Connection, active, State = #ofsh_logic_state{
                                            generation_id = GenId,
                                            main_connection = Connection,
                                            version = Version})) ->
    Msg = set_role(Version, master, GenId),
    State1 = State#ofsh_logic_state{generation_id = next_generation_id(GenId)},
    case of_driver:sync_send(Connection, Msg) of
        {error, Reason} ->
            {error, Reason, State1};
        {ok, _Reply} ->
            {ok, State1}
    end;
tell_controller_mode(Connection, standby, State) ->

my_controller_mode(Peer, DataPathId) ->
    case gen_server:call({ofs_handler, Peer}, {get_controller_state, DataPathId}) of
        active_in_recovery ->
        active ->
        standby_in_recovery ->
        standby ->
    end.
