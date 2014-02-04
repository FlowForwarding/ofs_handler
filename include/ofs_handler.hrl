%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%%-------------------------------------------------------------------

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
