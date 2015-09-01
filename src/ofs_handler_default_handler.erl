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
%%% Default callback handler for ofs_handler callbacks.  Returns
%%% errors on connection attempts.
%%% @end
-module(ofs_handler_default_handler).
-copyright("2013, Erlang Solutions Ltd.").

% default callback handler for ofs_handler

-behaviour(ofs_handler).

-export([init/7,
         connect/8,
         disconnect/1,
         failover/2,
         handle_message/2,
         handle_error/2,
         terminate/1]).

% ofs_handler callbacks
init(_Handler, _IpAddr, _DatapathId, _Features, _Version, _Connection, _Opt) ->
    {error, no_handler}.

connect(_Handler, _IpAddr, _DatapathId, _Features, _Version, _Connection, _AuxId, _Opt) ->
    {error, no_handler}.

% default handler rejects connections
% these callbacks are never called

disconnect(_State) ->
    ok.

failover(_ActiveState, StandbyState) ->
    {ok, StandbyState}.

handle_message(_Message, _State) ->
    ok.

handle_error(_Reason, _State) ->
    ok.

terminate(_State) ->
    ok.
