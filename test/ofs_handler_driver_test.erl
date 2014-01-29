-module(ofs_handler_driver_test).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun init_calls_handler/0
           ,fun connect_calls_handler/0
           ,fun message_calls_handler/0
           ,fun error_calls_handler/0
           ,fun disconnect_calls_handler/0
           ,fun terminate_calls_handler/0
        ]
    }.

setup() ->
    meck:new(ofs_handler_logic),
    ok.

cleanup(ok) ->
    meck:unload(ofs_handler_logic),
    ok.

-define(PID, handler_pid).
-define(IPADDR, {1,2,3,4}).
-define(DATAPATHID, <<"datapathid">>).
-define(FEATURES, {features, here}).
-define(VERSION, 4).
-define(CONNECTION, connection_pid).
-define(CONNECTIONPID, connection_handler_pid).
-define(AUXID, 2).
-define(OPT, [{list, the}, {options, here}]).
-define(STATE, {some, state, variable}).
-define(MESSAGE, {some, message}).
-define(ERROR, {some, error}).
-define(REASON, some_reason).

init_calls_handler() ->
    meck:expect(ofs_handler_logic, ofd_find_handler, fun(?DATAPATHID) -> {ok, ?PID} end),
    meck:expect(ofs_handler_logic, ofd_init, fun(?PID, ?IPADDR, ?DATAPATHID, ?FEATURES, ?VERSION, ?CONNECTION, ?OPT) -> {ok, ?CONNECTIONPID} end),
    {ok, _State} = ofs_handler_driver:init(?IPADDR, ?DATAPATHID, ?FEATURES, ?VERSION, ?CONNECTION, ?OPT),
    ?assert(meck:validate(ofs_handler_logic)).

connect_calls_handler() ->
    meck:expect(ofs_handler_logic, ofd_find_handler, fun(?DATAPATHID) -> {ok, ?PID} end),
    meck:expect(ofs_handler_logic, ofd_connect, fun(?PID, ?IPADDR, ?DATAPATHID, ?FEATURES, ?VERSION, ?CONNECTION, ?AUXID, ?OPT) -> {ok, ?CONNECTIONPID} end),
    {ok, _State} = ofs_handler_driver:handle_connect(?IPADDR, ?DATAPATHID, ?FEATURES, ?VERSION, ?CONNECTION, ?AUXID, ?OPT),
    ?assert(meck:validate(ofs_handler_logic)).

message_calls_handler() ->
    {ok, State} = ofd_init(),
    meck:expect(ofs_handler_logic, ofd_message, fun(?CONNECTIONPID, ?CONNECTION, ?MESSAGE) -> ok end),
    {ok, _State} = ofs_handler_driver:handle_message(?MESSAGE, State),
    ?assert(meck:validate(ofs_handler_logic)).

error_calls_handler() ->
    {ok, State} = ofd_init(),
    meck:expect(ofs_handler_logic, ofd_error, fun(?CONNECTIONPID, ?CONNECTION, ?ERROR) -> ok end),
    {ok, _State} = ofs_handler_driver:handle_error(?ERROR, State),
    ?assert(meck:validate(ofs_handler_logic)).

disconnect_calls_handler() ->
    {ok, State} = ofd_init(),
    meck:expect(ofs_handler_logic, ofd_disconnect, fun(?PID, ?CONNECTIONPID, ?CONNECTION, ?REASON) -> ok end),
    ok = ofs_handler_driver:handle_disconnect(?REASON, State),
    ?assert(meck:validate(ofs_handler_logic)).

terminate_calls_handler() ->
    {ok, State} = ofd_init(),
    meck:expect(ofs_handler_logic, ofd_terminate, fun(?PID, ?CONNECTIONPID, ?CONNECTION, ?REASON) -> ok end),
    ok = ofs_handler_driver:terminate(?REASON, State),
    ?assert(meck:validate(ofs_handler_logic)).

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

ofd_init() ->
    meck:expect(ofs_handler_logic, ofd_find_handler, fun(?DATAPATHID) -> {ok, ?PID} end),
    meck:expect(ofs_handler_logic, ofd_init, fun(?PID, ?IPADDR, ?DATAPATHID, ?FEATURES, ?VERSION, ?CONNECTION, ?OPT) -> {ok, ?CONNECTIONPID} end),
    ofs_handler_driver:init(?IPADDR, ?DATAPATHID, ?FEATURES, ?VERSION, ?CONNECTION, ?OPT).
