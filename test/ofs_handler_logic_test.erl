-module(ofs_handler_logic_test).

-export([packet_filter/1]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(V4, 4).
-define(APPS, [compiler, syntax_tools, mnesia, xmerl, ofs_handler]).
-define(IPADDR, {192,168,0,44}).
-define(DATAPATHID, <<"datapathid">>).
-define(CONNECTION, connection_pid).
-define(CALLBACK_OPT, [{call, back}, {options, here}]).
-define(OPT, [{callback_module, ofs_handler_default_handler}, {callback_opt, ?CALLBACK_OPT}]).
-define(MESSAGE, #ofp_message{}).
-define(FEATURES, #ofp_message{}).
-define(CALLBACK_STATE, [{callback, state}, {goes, here}]).
-define(REPLY, {ok, #ofp_message{}}).
-define(ECHO_REQUEST, of_msg_lib:echo_request(?VERSION, <<"hello">>)).
-define(MATCH_DATA, <<"AAAmatchingdata">>).
-define(NO_MATCH_DATA, <<"BBBnomatchingdata">>).
-define(PKIN_BUFFER_ID, no_buffer).
-define(PKIN_REASON, action).
-define(PKIN_TABLE_ID, 1).
-define(PKIN_COOKIE, <<-1:64>>).
-define(PKIN_MATCH, #ofp_match{}).

all_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun send_message/0,
            fun send_list_message/0,
            fun sync_send_message/0,
            fun sync_send_list_message/0,
            fun subscribe/0,
            fun message_echo_request/0,
            fun message_packet_in/0
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


    ?assert(meck:called(of_driver, send, ['_','_'])),
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

    ?assert(meck:called(of_driver, send_list, ['_','_'])),
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

    ?assert(meck:called(of_driver, sync_send, ['_','_'])),
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

    ?assert(meck:called(of_driver, sync_send_list, ['_','_'])),
    ?assert(meck:validate(of_driver)),
    ?assert(meck:validate(ofs_handler_default_handler)),

    unload_mocks(),
    ok.

subscribe() ->
    mock_of_driver(),
    mock_callback_handler(),
    ofs_handler_init(),

    ok = ofs_handler:subscribe(?DATAPATHID, test_receiver, echo_request),
    ok = ofs_handler:subscribe(?DATAPATHID, test_receiver, {packet_in, fun test_receiver:filterfn/1}),
    ?assertEqual(
        lists:sort([{echo_request, test_receiver, true},
         {packet_in, test_receiver, fun test_receiver:filterfn/1}]),
        lists:sort(ofs_handler:get_subscriptions(?DATAPATHID, test_receiver))),

    ok = ofs_handler:unsubscribe(?DATAPATHID, test_receiver, echo_request),
    ?assertEqual(
        [{packet_in, test_receiver, fun test_receiver:filterfn/1}],
        ofs_handler:get_subscriptions(?DATAPATHID, test_receiver)),

    ok = ofs_handler:unsubscribe(?DATAPATHID, test_receiver, {packet_in, fun test_receiver:filterfn/1}),
    ?assertEqual(
        [],
        ofs_handler:get_subscriptions(?DATAPATHID, test_receiver)),

    ?assert(meck:validate(of_driver)),
    ?assert(meck:validate(ofs_handler_default_handler)),

    unload_mocks(),
    ok.

message_echo_request() ->
    % receive echo request
    mock_of_driver(),
    mock_callback_handler(),
    mock_test_receiver(),
    {ok, OfDriverCBState} = ofs_handler_init(),
    meck:expect(test_receiver, handle_message,
                fun(Msg, CallbackState) ->
                    ?assertEqual(of_msg_lib:decode(?ECHO_REQUEST), Msg),
                    ?assertEqual(?CALLBACK_STATE, CallbackState),
                    {ok, CallbackState}
                end),

    ok = ofs_handler:subscribe(?DATAPATHID, test_receiver, echo_request),
    {ok, OfDriverCBState} = ofs_handler_driver:handle_message(?ECHO_REQUEST, OfDriverCBState),
    ok = ofs_handler:unsubscribe(?DATAPATHID, test_receiver, echo_request),

    ?assert(meck:called(test_receiver, handle_message, ['_','_'])),
    ?assert(meck:validate(of_driver)),
    ?assert(meck:validate(ofs_handler_default_handler)),

    unload_mocks(),
    ok.

message_packet_in() ->
    % receive packet in, with filtering
    mock_of_driver(),
    mock_callback_handler(),
    mock_test_receiver(),
    {ok, OfDriverCBState} = ofs_handler_init(),
    meck:expect(test_receiver, handle_message,
                fun(Msg, CallbackState) ->
                    ?assertEqual(
                            of_msg_lib:decode(packet_in(?MATCH_DATA)), Msg),
                    ?assertEqual(?CALLBACK_STATE, CallbackState),
                    {ok, CallbackState}
                end),

    ok = ofs_handler:subscribe(?DATAPATHID, test_receiver, {packet_in, fun ?MODULE:packet_filter/1}),
    {ok, OfDriverCBState} = ofs_handler_driver:handle_message(packet_in(?NO_MATCH_DATA), OfDriverCBState),
    ?assertNot(meck:called(test_receiver, handle_message, ['_','_'])),

    {ok, OfDriverCBState} = ofs_handler_driver:handle_message(packet_in(?MATCH_DATA), OfDriverCBState),
    ?assert(meck:called(test_receiver, handle_message, ['_','_'])),

    ok = ofs_handler:unsubscribe(?DATAPATHID, test_receiver, {packet_in, fun ?MODULE:packet_filter/1}),

    ?assert(meck:validate(of_driver)),
    ?assert(meck:validate(ofs_handler_default_handler)),
    ok.

%% ----------------------------------------------------------------------------
%% Test callback functions
%% ----------------------------------------------------------------------------

packet_filter({packet_in, _, Fields}) ->
    Data = proplists:get_value(data, Fields),
    Data =:= ?MATCH_DATA.

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

mock_test_receiver() ->
    meck:new(test_receiver, [passthrough]).

unload_mocks() ->
    meck:unload(of_driver),
    meck:unload(ofs_handler_default_handler).

ofp_message(OfMsg) ->
    #ofp_message{version = ?V4, body = OfMsg}.

packet_in(Data) ->
    ofp_message(#ofp_packet_in{
        buffer_id = ?PKIN_BUFFER_ID,
        reason = ?PKIN_REASON,
        table_id = ?PKIN_TABLE_ID,
        cookie = ?PKIN_COOKIE,
        match = ?PKIN_MATCH,
        data = Data}).
