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
    get_features/1,
    get_switch_config/1,
    get_switch_description/1,
    set_switch_config/3,
    send_packet/4,
    delete_all_flows/1,
    modify_flow/2,
    modify_flows/2,
    deleted_all_groups/1,
    modify_group/2,
    modify_groups/2,
    modify_port/2,
    modify_ports/2,
    modify_meter/2,
    modify_meters/2,
    get_flow_statistics/4,
    get_aggregate_statistics/4,
    get_table_statistics/1,
    get_port_statistics/2,
    get_queue_statistics/3,
    get_group_statistics/2,
    get_meter_statistics/2,
    get_table_features/1,
    get_auth_table_features/1,
    set_table_features/2,
    get_port_descriptions/1,
    get_auth_port_descriptions/1,
    get_group_descriptions/1,
    get_auth_group_descriptions/1,
    get_group_features/1,
    get_meter_configuration/2,
    get_auth_meter_configuration/2,
    get_meter_features/1,
    get_flow_descriptions/1,
    get_auth_flow_descriptions/1,
    experimenter/4,
    barrier/1,
    get_queue_configuration/2,
    get_auth_queue_configuration/2,
    set_connection_role/3,
    get_async_message_delivery_config/1,
    set_async_message_delivery_config/4,
    ping_switch/1,
    subscribe_async_message/3,
    subscribe_async_messages/3,
    get_subscribtions_async_messages/2,
    terminate/1
]).

%%% ===========================================================================
%%% Public API
%%% ===========================================================================

get_features(DataPathId) ->
    call_active(DataPathId, get_features).

get_switch_config(DataPathId) ->
    % maps to OF get_config
    call_active(DataPathId, get_config).

get_switch_description(DataPathId) ->
    % maps to OF get_description
    call_active(DataPathId, get_description).

set_switch_config(DataPathId, ConfigFlag, MissSendLength) ->
    % maps to OF set_config
    call_active(DataPathId, {set_config, ConfigFlag, MissSendLength}).

send_packet(DataPathId, Data, PortNumber, Actions) ->
    call_active(DataPathId, {send_packet, Data, PortNumber, Actions}).

delete_all_flows(DataPathId) ->
    call_active(DataPathId, delete_all_flows).

modify_flow(DataPathId, FlowMod) ->
    modify_flows(DataPathId, [FlowMod]).

modify_flows(DataPathId, FlowMods) ->
    call_active(DataPathId, {modify_flows, FlowMods}).

deleted_all_groups(DataPathId) ->
    call_active(DataPathId, deleted_all_groups).

modify_group(DataPathId, GroupMod) ->
    modify_groups(DataPathId, [GroupMod]).

modify_groups(DataPathId, GroupMods) ->
    call_active(DataPathId, {modify_groups, GroupMods}).

modify_port(DataPathId, PortMod) ->
    modify_ports(DataPathId, [PortMod]).

modify_ports(DataPathId, PortMods) ->
    call_active(DataPathId, {modify_ports, PortMods}).

modify_meter(DataPathId, MeterMod) ->
    modify_meters(DataPathId, [MeterMod]).

modify_meters(DataPathId, MeterMods) ->
    call_active(DataPathId, {modify_meters, MeterMods}).

get_flow_statistics(DataPathId, TableId, Matches, Options) ->
    call_active(DataPathId, {get_flow_statistics, TableId, Matches, Options}).

get_aggregate_statistics(DataPathId, TableId, Matches, Options) ->
    call_active(DataPathId, {get_aggregate_statistics,
                                                TableId, Matches, Options}).

get_table_statistics(DataPathId) ->
    call_active(DataPathId, get_table_statistics).

get_port_statistics(DataPathId, PortNumber) ->
    call_active(DataPathId, {get_port_statistics, PortNumber}).

get_queue_statistics(DataPathId, PortNumber, QueueId) ->
    call_active(DataPathId, {get_queue_statistics, PortNumber, QueueId}).

get_group_statistics(DataPathId, GroupId) ->
    call_active(DataPathId, {get_group_statistics, GroupId}).

get_meter_statistics(DataPathId, MeterId) ->
    call_active(DataPathId, {get_meter_statistics, MeterId}).

get_table_features(DataPathId) ->
    call_active(DataPathId, get_table_features).

get_auth_table_features(DataPathId) ->
    call_active(DataPathId, get_auth_table_features).

set_table_features(DataPathId, TableFeatures) ->
    call_active(DataPathId, {set_table_features, TableFeatures}).

get_port_descriptions(DataPathId) ->
    call_active(DataPathId, get_port_descriptions).

get_auth_port_descriptions(DataPathId) ->
    call_active(DataPathId, get_auth_port_descriptions).

get_group_descriptions(DataPathId) ->
    call_active(DataPathId, get_group_descriptions).

get_auth_group_descriptions(DataPathId) ->
    call_active(DataPathId, get_auth_group_descriptions).

get_group_features(DataPathId) ->
    call_active(DataPathId, get_group_features).

get_meter_configuration(DataPathId, MeterId) ->
    call_active(DataPathId, {get_meter_configuration, MeterId}).

get_auth_meter_configuration(DataPathId, MeterId) ->
    call_active(DataPathId, {get_auth_meter_configuration, MeterId}).

get_meter_features(DataPathId) ->
    call_active(DataPathId, get_meter_features).

get_flow_descriptions(DataPathId) ->
    % maps to OF uses get_table_statistics
    call_active(DataPathId, get_flow_descriptions).

get_auth_flow_descriptions(DataPathId) ->
    call_active(DataPathId, get_auth_flow_descriptions).

experimenter(DataPathId, ExpId, Type, Data) ->
    call_active(DataPathId, {experimenter, ExpId, Type, Data}).

barrier(DataPathId) ->
    call_active(DataPathId, barrier).

get_queue_configuration(DataPathId, PortNumber) ->
    call_active(DataPathId, {get_queue_configuration, PortNumber}).

get_auth_queue_configuration(DataPathId, PortNumber) ->
    call_active(DataPathId, {get_queue_auth_configuration, PortNumber}).

set_connection_role(DataPathId, Role, GenerationId) ->
    % maps to OF set_role
    call_active(DataPathId, {set_role, Role, GenerationId}).

get_async_message_delivery_config(DataPathId) ->
    % maps to OF get_async_configuration
    call_active(DataPathId, get_async_configuration).

set_async_message_delivery_config(DataPathId, PacketInMask, PortStatusMask, FlowRemoveMask) ->
    % maps to OF set_async_configuration
    call_active(DataPathId, {set_async_configuration, PacketInMask, PortStatusMask, FlowRemoveMask}).

ping_switch(DataPathId) ->
    % send echo, wait for response
    call_active(DataPathId, ping_switch).

subscribe_async_message(DataPathId, Module, Item) ->
    subscribe_async_message(DataPathId, Module, [Item]).

subscribe_async_messages(DataPathId, Module, Items) ->
    call_active(DataPathId, {async_subscribe, Module, Items}).

get_subscribtions_async_messages(DataPathId, Module) ->
    call_active(DataPathId, {get_async_subscribe, Module}).

terminate(DataPathId) ->
    call_active(DataPathId, terminate).

%%% ===========================================================================
%%% Internal functions
%%% ===========================================================================

call_active_handler(DataPathId, Command) ->
    Handler = locate_active_handler(DataPathId),
    gen_server:call(Handler, Command).

locate_active_handler(DataPathId) ->
    ofs_handler_locator:locate_handler(active, DataPathId).
