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
%%% Handle callbacks from of_driver.
%%% @end
-module(ofs_handler_driver).
-copyright("2013, Erlang Solutions Ltd.").

-include_lib("ofs_handler/include/ofs_handler_logger.hrl").

% -type ip_address() :: string().
% -type datapath_id() :: binary().
% -type of_version() :: integer().
% -type of_connection() :: term().
% -type ofd_options() :: term().
% -type ofs_error_reason() :: term().

-export([
    init/6,
    handle_connect/7,
    handle_message/2,
    handle_error/2,
    handle_disconnect/2,
    terminate/2
]).

-define(STATE, ofs_handler_driver_state).
-record(?STATE, {
    handler_pid :: pid(),
    message_pid :: pid(),
    connection
    }).

% of_driver callbacks
init(IpAddr, DataPathId, Features, Version, Connection, Opt) ->
    {ok, Pid} = ofs_handler_logic:ofd_find_handler(DataPathId),
    {ok, MessagePid} = ofs_handler_logic:ofd_init(Pid,
                    IpAddr, DataPathId, Features, Version, Connection, Opt),
    {ok, #?STATE{handler_pid = Pid, message_pid = MessagePid, connection = Connection}}.

handle_connect(IpAddr, DataPathId, Features, Version, Connection, AuxId, Opt) ->
    {ok, Pid} = ofs_handler_logic:ofd_find_handler(DataPathId),
    {ok, MessagePid} = ofs_handler_logic:ofd_connect(Pid,
                IpAddr, DataPathId, Features, Version, Connection, AuxId, Opt),
    {ok, #?STATE{handler_pid = Pid, message_pid = MessagePid, connection = Connection}}.

handle_message(Msg, State = #?STATE{
                                message_pid = MessagePid,
                                connection = Connection}) ->
    case ofs_handler_logic:ofd_message(MessagePid, Connection, Msg) of
        ok ->
            {ok, State};
        {terminate, Reason} ->
            {terminate, Reason, State}
    end.

handle_error(Error, State = #?STATE{
                                message_pid = MessagePid,
                                connection = Connection}) ->
    case ofs_handler_logic:ofd_error(MessagePid, Connection, Error) of
        ok ->
            {ok, State};
        {terminate, Reason} ->
            {terminate, Reason, State}
    end.

handle_disconnect(Reason, #?STATE{
                                handler_pid = Pid,
                                message_pid = MessagePid,
                                connection = Connection}) ->
    ok = ofs_handler_logic:ofd_disconnect(Pid, MessagePid, Connection, Reason).

terminate(Reason, #?STATE{
                                handler_pid = Pid,
                                message_pid = MessagePid,
                                connection = Connection}) ->
    ok = ofs_handler_logic:ofd_terminate(Pid, MessagePid, Connection, Reason).
