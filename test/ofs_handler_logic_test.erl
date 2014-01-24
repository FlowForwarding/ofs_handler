-module(ofs_handler_logic_test).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(V4, 4).

-define(APPS, [compiler, syntax_tools, mnesia, xmerl, ofs_handler]).
-define(IPADDR, {192,168,0,44}).
-define(DATAPATHID, <<"datapathid">>).
-define(VERSION, ?V4).
-define(CONNECTION, connection_pid).
-define(CALLBACK_OPT, [{call, back}, {options, here}]).
-define(OPT, [{callback_module, ofs_handler_default_handler}, {callback_opt, ?CALLBACK_OPT}]).
-define(MESSAGE, #ofp_message{}).
-define(FEATURES, #ofp_message{}).
-define(CALLBACK_STATE, [{callback, state}, {goes, here}]).
-define(REPLY, {ok, #ofp_message{}}).

all_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun send_message/0,
            fun send_list_message/0,
            fun sync_send_message/0,
            fun sync_send_list_message/0
        ]
    }.

setup() ->
    start_apps(),
    ok.

cleanup(ok) ->
    stop_apps(),
    ok.

send_message() ->
    mock_of_driver(),
    mock_callback_handler(),
    meck:expect(of_driver, send, fun(?CONNECTION, ?MESSAGE) -> ok end),

    ofs_handler_init(),
    ok = ofs_handler:send(?DATAPATHID, ?MESSAGE),

    ?assert(meck:validate(of_driver)),
    ?assert(meck:validate(ofs_handler_default_handler)),

    unload_mocks(),
    ok.

send_list_message() ->
    mock_of_driver(),
    mock_callback_handler(),
    meck:expect(of_driver, send_list, fun(?CONNECTION, [?MESSAGE, ?MESSAGE]) -> ok end),

    ofs_handler_init(),
    ok = ofs_handler:send_list(?DATAPATHID, [?MESSAGE, ?MESSAGE]),

    ?assert(meck:validate(of_driver)),
    ?assert(meck:validate(ofs_handler_default_handler)),

    unload_mocks(),
    ok.

sync_send_message() ->
    mock_of_driver(),
    mock_callback_handler(),
    meck:expect(of_driver, sync_send, fun(?CONNECTION, ?MESSAGE) -> {ok, ?REPLY} end),

    ofs_handler_init(),
    {ok, ?REPLY} = ofs_handler:sync_send(?DATAPATHID, ?MESSAGE),

    ?assert(meck:validate(of_driver)),
    ?assert(meck:validate(ofs_handler_default_handler)),

    unload_mocks(),
    ok.

sync_send_list_message() ->
    mock_of_driver(),
    mock_callback_handler(),
    meck:expect(of_driver, sync_send_list, fun(?CONNECTION, [?MESSAGE, ?MESSAGE]) -> {ok, [{ok, ?REPLY}, {ok, ?REPLY}]} end),

    ofs_handler_init(),
    {ok, [{ok, ?REPLY}, {ok, ?REPLY}]} = ofs_handler:sync_send_list(?DATAPATHID, [?MESSAGE, ?MESSAGE]),

    ?assert(meck:validate(of_driver)),
    ?assert(meck:validate(ofs_handler_default_handler)),

    unload_mocks(),
    ok.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

start_apps() ->
    error_logger:tty(false),
    [ok = application:start(A) || A <- ?APPS].

stop_apps() ->
    [ok = application:stop(A) || A <- lists:reverse(?APPS)].

ofs_handler_init() ->
    ofs_handler_driver:init(?IPADDR, ?DATAPATHID, ?FEATURES, ?VERSION, ?CONNECTION, ?OPT).

mock_of_driver() ->
    meck:new(of_driver),
    meck:expect(of_driver, close_connection, fun(?CONNECTION) -> ok end).

mock_callback_handler() ->
    meck:new(ofs_handler_default_handler),
    meck:expect(ofs_handler_default_handler, init, fun(active, ?IPADDR, ?DATAPATHID, ?FEATURES, ?VERSION, ?CONNECTION, ?CALLBACK_OPT) -> {ok, ?CALLBACK_STATE} end),
    meck:expect(ofs_handler_default_handler, terminate, fun(_) -> ok end).

unload_mocks() ->
    meck:unload(of_driver),
    meck:unload(ofs_handler_default_handler).
