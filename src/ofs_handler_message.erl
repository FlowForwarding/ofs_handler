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
%%% Process messages received from switch.
%%% @end
-module(ofs_handler_message).
-copyright("2013, Erlang Solutions Ltd.").

-include("ofs_handler_logic.hrl").
-include_lib("ofs_handler/include/ofs_handler_logger.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(STATE, ofs_handler_message_state).
-record(?STATE,
    {
        connection,
        auxid,
        callback_mod,
        callback_state,
        subscriptions
    }
).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/5,
         disconnect/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Connection, AuxId, CallbackMod, CallbackState, Subscriptions) ->
    gen_server:start_link(?MODULE,
            [Connection, AuxId, CallbackMod, CallbackState, Subscriptions], []).

disconnect(Pid, Reason) ->
    gen_server:call(Pid, {disconnect, Reason}).

%% ------------------------------------------------------------------
%% utility functions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Connection, AuxId, CallbackMod, CallbackState, Subscriptions]) ->
    {ok, #?STATE{connection = Connection,
                 auxid = AuxId,
                 callback_mod = CallbackMod,
                 callback_state = CallbackState,
                 subscriptions = Subscriptions}}.

% handle API
handle_call(terminate, _From, State) ->
    signal_stop(terminated_by_call),
    {reply, ok, State}; 
% handle callbacks from of_driver
handle_call({error, _Connection, Reason}, _From,
                State = #?STATE{callback_state = CallbackState,
                                callback_mod = Module}) ->
    % error on the connection
    do_callback(Module, handle_error, [Reason, CallbackState]),
    {reply, ok, State};
handle_call({message, _Connection, Message}, _From, State) ->
    % switch sent us a message
    case notify(Message, State) of
        ok ->
            ok;
        {terminate, Reason} ->
            signal_stop(Reason)
    end,
    {reply, ok, State};
handle_call({disconnect, Reason}, _From, State) ->
    % connection closed
    signal_stop(Reason),
    {reply, ok, State#?STATE{connection = undefined}};
handle_call(_Request, _From, State) ->
    % unknown request
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #?STATE{connection = Connection,
                          auxid = AuxId,
                          callback_state = CallbackState,
                          callback_mod = Module}) ->
    ?WARNING("[~p] terminate reason(~p)", [?MODULE, Reason]),
    close_connection(Connection),
    case AuxId of
        main ->
            do_callback(Module, terminate, [CallbackState]);
        _ ->
            do_callback(Module, disconnect, [CallbackState])
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

close_connection(undefined) ->
    % connection is already closed
    ok;
close_connection(Connection) ->
    of_driver:close_connection(Connection).

signal_stop(Reason) ->
    ?INFO("ofs_handler(~p) stopping: ~p", [self(), Reason]),
    gen_server:cast(self(), stop).

do_callback(Module, Function, Args) ->
    erlang:apply(Module, Function, Args).

notify(Message, State) ->
    Subscriptions = State#?STATE.subscriptions,
    CallbackState = State#?STATE.callback_state,
    {MsgType, _, _} = DecodedMsg = of_msg_lib:decode(Message),
    Subscribers = ets:lookup(Subscriptions, MsgType),
    notify_subscriber(Subscribers, DecodedMsg, CallbackState).

notify_subscriber([], _Message, _CallbackState) ->
    ok;
notify_subscriber([{_Type, Module, Filter}|Rest], DecodedMsg, CallbackState) ->
    case subscriber_filter(Filter, DecodedMsg) of
        true ->
            case do_callback(Module, handle_message, [DecodedMsg, CallbackState]) of
                ok ->
                    notify_subscriber(Rest, DecodedMsg, CallbackState);
                {terminate, Reason} ->
                    {error, Reason}
            end;
        _ ->
            notify_subscriber(Rest, DecodedMsg, CallbackState)
    end.

subscriber_filter(true, _DecodedMsg) -> true;
subscriber_filter(Fn, DecodedMsg) when is_function(Fn) ->
    % catch errors?
    Fn(DecodedMsg).
