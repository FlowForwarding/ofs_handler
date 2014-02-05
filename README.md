ofs_handler
===========
# OpenFlow Switch Handler

ofs_handler provides a higher level, somewhat abstracted interface to
an OpenFlow switch.  It manages of_driver connections to the
OpenFlow switches and identifies switches using their datapath ID.  You
can also subscribe to messages received from the switches and apply
a filter function to the subscription.

## Connecting to of_driver
To use ofs_handler with of_driver, set the of_driver callback module
to ofs_handler_driver in the of_driver environment in sys.config:

```
{of_driver,[{listen_ip, {0,0,0,0}},
            {listen_port, 6633},
            {listen_opts,[binary, {packet, raw}, {active, false}, {reuseaddr, true}]},
            {of_compatible_versions, [4]},
            {callback_module, ofs_handler_driver},
            {init_opt,[]}
 ]}
```

## ofs_handler callbacks
To use ofs_handler with your code, set the ofs_handler callback module
to your callback module.  This module must implement the ofs_handler
callback functions.
```
{ofs_handler,[
        {callback_module,simple_ne_ofsh},
        {callback_opts,[]}
 ]},
```
Set callback_opts to the Erlang term you want ofs_handler to pass
to the init and connect callback functions.

## Callback functions
When switch makes the main connection, ofs_handler calls the init
function in your callback module.  Similarly, when the switch makes
an auxiliary connection, ofs_handler calls the connect callback
function in your callback module.  Your init and connect functions
can return a state variable.  There is one state variable for each
connection.  This state variable is then used an argument to
subsequent callback functions related to that connection.  For
example, when ofs_handler receives a message from the switch on the
main connection (if the appropriate subscriptions are in place)
ofs_handler calls handle_message with the state variable returned
by init.  Your state variable may want to include the datapath ID
and other identifying information for the connection.  For example,
if you start a process for each connection, you can capture the pid
of that process in the state.

### Callback summary
1. init (new main connection)
2. connect (new auxiliary connections)
3. handle_message (messages received from the switch)
4. disconnect (auxiliary connection closed)
5. terminate (main connection closed)

## Subscriptions
Your code can create subscriptions to messages received from the switch.
Messages are identified by their message type as described in
of_msg_lib.  You must create subscriptions for replies and
asynchronous messages.  Example message types:

- features_reply
- config_reply
- desc_reply
- table_features_reply
- echo_reply
- flow_stats_reply
- packet_in
- flow_removed

Example subscription:
```
ofs_handler:subscribe(DatapathId, message_callback_module, echo_reply).
```
When ofs_handler receives an echo_reply message from the switch
with datapath ID DatapathId, ofs_handler calls
message_callback_module:handle_message/2.

The callback module
for subscriptions does not need to be same as the callback module for the other
ofs_handler callback functions.  You may create more than one subscription
for the same message type.  ofs_handler calls the handle_message callback
function in every subscribing module.

You may also specify a filtering function to subscription by using
a tuple of the message type and filter function.  The filter function
should take one argument which is the message.  If the filter
function returns true, ofs_handler calls the handle_message callback
function for the subscribing module.  If the filter function returns
false, ofs_handle does not do the callback.  The filter function
does not affect any other subscription.

Example filtered subscription:
```
ofs_handler:subscribe(DatapathId, message_callback_module,
                            {packet_in, fun filter_module:filter_fun/1}).
```
ofs_handler calls message_callback_module:handle_message/2 when it
receives a packet_in message from the switch with datapath ID
DatapathId and filter_module:filter_fun/1 returns true.
