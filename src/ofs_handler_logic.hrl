%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%%-------------------------------------------------------------------
-copyright("2013, Erlang Solutions Ltd.").

-define(HANDLERS_TABLE, ofs_handler_logic_pids).
-record(handlers_table, {
    datapath_id,
    handler_pid
}).
