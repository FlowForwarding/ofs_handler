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

-type datapath_mac() :: binary().
-type datapath_lsid() :: integer().
-type datapath_id() :: {datapath_lsid(), datapath_mac()}.
-type error_reason() :: atom().
-type subscription_type() :: atom().
-type subscription_filter() :: function().
-type subscription_item() :: subscription_type() | {subscription_type(), subscription_filter()}.

-type handler_mode() :: active | standby.
-type ipaddress() :: {integer(), integer(), integer(), integer()}.
-type of_version() :: integer().
-type connection() :: term().
-type options() :: term().
-type features() :: term().
-type auxid() :: integer().
