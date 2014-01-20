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
% Start a ofs_handler process for the switch identified by DataPathId.
% @end
start_link(DataPathId) ->
    ofs_handler_sup:start_child(DataPathId).

% @doc
% @end
send(DatapathId, Msg) ->
    call_active(DatapathId, {send, Msg}).

sync_send(DatapathId, Msg) ->
    call_active(DatapathId, {sync_send, Msg}).

send_list(DatapathId, Msgs) ->
    call_active(DatapathId, {send_list, Msgs}).

sync_send_list(DatapathId, Msgs) ->
    call_active(DatapathId, {sync_send_list, Msgs}).

ping_switch(DatapathId) ->
    % send echo, wait for response
    call_active(DatapathId, ping_switch).

subscribe_async_message(DatapathId, Module, Item) ->
    call_active(DatapathId, {async_subscribe, Module, Item}).

get_subscriptions_async_messages(DatapathId, Module) ->
    call_active(DatapathId, {get_async_subscribe, Module}).

terminate(DataPathId) ->
    call_active(DataPathId, terminate).

%%% ===========================================================================
%%% Internal functions
%%% ===========================================================================

call_active(DataPathId, Command) ->
    Handler = locate_active(DataPathId),
    gen_server:call(Handler, Command).

locate_active(DataPathId) ->
    ofs_handler_locator:locate_handler(active, DataPathId).
