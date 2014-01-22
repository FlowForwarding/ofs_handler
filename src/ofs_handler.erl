%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc 
%%% API for ofs_handler
%%% @end
%%%-------------------------------------------------------------------
-module(ofs_handler).
-copyright("2013, Erlang Solutions Ltd.").

-export([
    start_link/1,
    send/2,
    send_list/2,
    sync_send/2,
    sync_send_list/2,
    ping_switch/1,
    subscribe_async_message/3,
    get_subscriptions_async_messages/2,
    terminate/1
]).

%%% ===========================================================================
%%% Public API
%%% ===========================================================================

% @doc
% Start a ofs_handler process for the switch identified by DatapathId.
% @end
start_link(DatapathId) ->
    ofs_handler_logic_sup:start_child(DatapathId).

% @doc
% @end
send(DatapathId, Msg) ->
    ofs_handler_logic:call_active(DatapathId, {send, Msg}).

sync_send(DatapathId, Msg) ->
    ofs_handler_logic:call_active(DatapathId, {sync_send, Msg}).

send_list(DatapathId, Msgs) ->
    ofs_handler_logic:call_active(DatapathId, {send_list, Msgs}).

sync_send_list(DatapathId, Msgs) ->
    ofs_handler_logic:call_active(DatapathId, {sync_send_list, Msgs}).

ping_switch(DatapathId) ->
    % send echo, wait for response
    ofs_handler_logic:call_active(DatapathId, ping_switch).

subscribe_async_message(DatapathId, Module, Item) ->
    ofs_handler_logic:call_active(DatapathId, {async_subscribe, Module, Item}).

get_subscriptions_async_messages(DatapathId, Module) ->
    ofs_handler_logic:call_active(DatapathId, {get_async_subscribe, Module}).

terminate(DatapathId) ->
    ofs_handler_logic:call_active(DatapathId, terminate).

%%% ===========================================================================
%%% Internal functions
%%% ===========================================================================
