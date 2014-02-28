%%------------------------------------------------------------------------------
%% Copyright 2014 FlowForwarding.org
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%-----------------------------------------------------------------------------

%% @author Erlang Solutions Ltd. <openflow@erlang-solutions.com>
%% @copyright 2014 FlowForwarding.org

%%% @doc 
%%% ofs_handler provides a higher level, somewhat abstracted interface
%%% to an OpenFlow switch.  It provides functions to send requests to
%%% the switch and subscribe to messages from the switch.  ofs_handler_driver
%%% should be used as the callback module with of_driver.  ofs_handler also
%%% has a callback interface.  The application environment variable
%%% `callback_module' is the default callback module.  `callback_opt' is
%%% a term() passed to the Module:init/7 and Module:connect/8 callback
%%% functions.  The Opt passed from of_driver overrides these values.
%%%
%%% ofs_handler always uses the main connection to send requests to the switch.
%%% It creates separate ofs_handler_message pids to handle messages
%%% received from the switch.  This allows for concurrent processing of
%%% asynchronous messages from the switch.
%%% 
%%% === of_driver Callbacks ===
%%% `ofs_handler_driver' is the of_driver callback module for ofs_handler.
%%%
%%% `ofs_handler_driver:init' - Accept the new main connection from the
%%% switch.  The Options passed from of_driver is a property list.  The
%%% keys 'callback_module' and 'callback_opt' override the values from
%%% ofs_handler's application environment.
%%%
%%% `ofs_handler_driver:handle_connect' - Accept a new auxiliary connection
%%% from the switch.  The Options passed from of_driver is a property list.
%%% The keys 'callback_module' and 'callback_opt' override the values from
%%% ofs_handler's application environment.
%%%
%%% `ofs_handler_driver:handle_message' - Accept message received from
%%% the switch.
%%%
%%% `ofs_handler_driver:handle_error' - Accept error received from the
%%% switch.
%%%
%%% `ofs_handler_driver:handle_disconnect' - Terminates handling of an
%%% auxiliary connection.
%%%
%%% `ofs_handler_driver:terminate' - Terminates handling of the main
%%% connection.  ofs_handler can no longer talk to the switch.
%%%
%%% === Callback Functions ===
%%% `Module:init(active, IpAddr, DatapathId, Features, Version,
%%% Connection, Opt) -> {ok, State} | {error, Reason}.' - Accept a new
%%% main connection.  `active' identifies it is the active ofs_handler
%%% that has received the new main connection.  `IpAddr' is the Ip
%%% address of the switch (`{A, B, C, D}' format), `DatapathId' is the
%%% datapath id of the switch, `Features' is the decoded `features_reply'
%%% message for the switch, `Version' is the protocol version, `Connection'
%%% is the of_driver connection, and `Opt' is the `callback_opt'.  The function
%%% returns `{ok, State}' where `State' is any Erlang term that is then
%%% passed to other callback functions.  The `State' is associated with
%%% the `Connection', so the callback functions may use `State' to identify
%%% the connection that triggered the callback.
%%%
%%% `Module:connect(active, IpAddr, DatapathId, Features, Version,
%%% Connection, AuxId, Opt) -> {ok, State} | {error, Reason}.' - Accept
%%% a new auxiliary connection.  The caller may assume that there is
%%% already a main connection from the same switch.  The parameters and
%%% return are the same as for `Module:init' with the addition of the
%%% `AuxId' parameter.  This is the auxiliary connection Id which was
%%% read from `Features'.
%%%
%%% `Module:disconnect(State) -> ok.' - Called when an auxiliary connection
%%% disconnects.  `State' is the Erlang term returned to ofs_handler
%%% from the `Module:connect' callback for the disconnecting auxiliary
%%% connection.  The callback module may want to store identifying information
%%% in `State'.
%%%
%%% `SubscriptionModule:handle_message(Msg, State) -> ok.' - Called when
%%% a message is received from the switch that matches a subscription.
%%% Note that the callback module for subscriptions may differ from
%%% module for the other callback functions.  Create subscriptions using
%%% `ofs_handler:subscribe/3'.
%%%
%%% `Module:handle_error(Reason, State) -> ok.' - Called when an error
%%% occurs on a connection.  'State' is the Erlang term returned to ofs_handler
%%% from `Module:init' or `Module:connect'.
%%%
%%% `Module:terminate(State) -> ok.' - Called when the main connection
%%% disconnects.  `State' is the Erlang term returned to ofs_handler
%%% from the `Module:init'.
%%% 
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
    subscribe/3,
    unsubscribe/3,
    get_subscriptions/2,
    terminate/1
]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include("ofs_handler.hrl").

%%% ===========================================================================
%%% Public API
%%% ===========================================================================

% @doc
% Start a ofs_handler process for the switch identified by DatapathId.
% @end
-spec start_link(datapath_id()) -> {ok, pid()}.
start_link(DatapathId) ->
    ofs_handler_logic_sup:start_child(DatapathId).

% @doc
% Send a request to the switch with the DatapathId.  The reply, if
% any, is returned via the Module:handle_message/2 callback.  An error
% occurs if there was a problem sending the request to the switch.  It
% does not indicate an error return from the switch.
% @end
-spec send(datapath_id(), ofp_message()) -> ok | {error, error_reason()}.
send(DatapathId, Msg) ->
    ofs_handler_logic:call_active(DatapathId, {send, Msg}).

% @doc
% Send a request to the switch with the DatapathId.  Implicitly
% adds a barrier request and waits for any replies.  The reply is
% returned to the caller as an ofp_message() that may be
% decoded using of_msg_lib:decode/1.  Returns `no_reply' if there
% was no reply.  An error occurs if there was
% a problem sending the request to the switch.  An error does not indicate
% an error response from the switch.
% @end
-spec sync_send(datapath_id(), ofp_message()) ->
                {ok, ofp_message() | noreply} | {error, error_reason()}.
sync_send(DatapathId, Msg) ->
    ofs_handler_logic:call_active(DatapathId, {sync_send, Msg}).

% @doc
% Send a list of requests to the switch with the DatapathId.  Replies,
% if any, are returned via the Module:handler_message/2 callback.  Returns
% `ok' if there are no errors.  Returns `error' if one or more requests
% resulted in an error.  The accompanying list has one element for
% each request in the request list in the corresponding order.
% Errors occur when there is a problem sending the
% request to the switch.  An error does not indicate an error response
% from the switch.
% @end
-spec send_list(datapath_id(), [ofp_message()]) ->
                                    ok | {error, [ok | error_reason()]}.
send_list(DatapathId, Msgs) ->
    ofs_handler_logic:call_active(DatapathId, {send_list, Msgs}).

% @doc
% Send a list of requests to the switch with the DatapathId.  Implicitly
% adds a barrier request after sending the list of requests and
% waits for any replies.  Replies are returned to the caller as ofp_message()
% records that may be decoded using of_msg_lib:decode/1.  The replies are
% returned in a list with one element for each request in the corresponding
% order.  Returns `no_reply' if the request had no reply.  An error
% occurs when there is a problem sending the request to the switch.  The
% accompanying list has one element for each request in the request list
% in the corresponding order.  An error does not indicate an error
% response from the switch.
% @end
-spec sync_send_list(datapath_id(), [ofp_message()]) ->
            {ok, [{ok, ofp_message() | no_reply}]} |
            {error, error_reason(),
                [{ok, ofp_message() | no_reply | {error, error_reason()}}]}.
sync_send_list(DatapathId, Msgs) ->
    ofs_handler_logic:call_active(DatapathId, {sync_send_list, Msgs}).

% @doc
% Send an echo request to the switch with the DatapathId
% and wait for a response. Returns `pong' if the switch is alive
% and `pang' if not.
% @end
-spec ping_switch(datapath_id()) -> pong | pang.
ping_switch(DatapathId) ->
    % send echo, wait for response
    ofs_handler_logic:call_active(DatapathId, ping_switch).

% @doc
% Add a callback subscription for asynchronous messages from the switch at
% DatapathId matching Item.  The callback function is in Module.  The
% item is a of_msg_lib message type (e.g., echo_reply) or a tuple with the
% message type and a filter function.
%
% If the Item is a message type atom, when the switch sends a message of that
% type to ofs_handler, ofs_handler calls Module:handle_message/2
% with the message.
%
%If the Item is a message type, filter function tuple, when the switch
% sends a message of that type to ofs_handler, ofs_handler calls the
% filter function with the message.  The message is a ofp_message record.
% If the filter function returns true, ofs_handler
% calls Module:handle_message/2 with the message and callback state
% returned by init or connect.  Otherwise, ofs_handler
% discards the message.
%
% Messages with no subscriptions are discarded.
%
% There may be any number of subscriptions to the same message type.
% ofs_handler calls Module:handle_message/2 for every matching subscription.
% @end
-spec subscribe(datapath_id(), module(), subscription_item()) -> ok.
subscribe(DatapathId, Module, Item) ->
    ofs_handler_logic:call_active(DatapathId, {async_subscribe, Module, Item}).

% @doc
% Remove a callback subscription for asynchronous messages from the switch
% at DatapathId for the matching Item.  After calling unsubscribe/3,
% Module will no longer recieve callbacks for Item.
% @end
-spec unsubscribe(datapath_id(), module(), subscription_item()) -> ok.
unsubscribe(DatapathId, Module, Item) ->
    ofs_handler_logic:call_active(DatapathId, {async_unsubscribe, Module, Item}).

% @doc
% Get the current list of callback subscriptions for the switch at DatapathId
% for Module.  FilterFunction is `true' if the subscription did not have
% a filter function.
% @end
-spec get_subscriptions(datapath_id(), module()) ->
            [{MessageType :: subscription_type(),
              Module :: module(),
              FilterFunction :: subscription_filter() | true}].
get_subscriptions(DatapathId, Module) ->
    ofs_handler_logic:call_active(DatapathId, {get_async_subscribe, Module}).

% @doc
% Terminate the ofs_handler for the switch at DatapathId.
% @end
-spec terminate(datapath_id()) -> ok.
terminate(DatapathId) ->
    ofs_handler_logic:call_active(DatapathId, terminate).
