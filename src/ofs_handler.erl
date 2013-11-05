%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Ruan Pienaar <ruan.pienaar@erlang-solutions.com>
%%% @doc 
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(ofs_handler).
-copyright("2013, Erlang Solutions Ltd.").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([ handle_connect/2,
	  handle_message/2,
	  handle_disconnect/2
	]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%------------------------------------------------------------------
-spec handle_connect(Switch :: term(), Version :: term()) -> ok | {error,Reason :: term()}.
handle_connect(Switch, Version) ->
    ok.

-spec handle_message(Switch :: term(), Version :: term()) -> ok | {error,Reason :: term()}.
handle_message(Switch, Msg) ->
    ok.

-spec handle_disconnect(Switch :: term(), Reason :: term()) -> ok | {error,Reason :: term()}.
handle_disconnect(Switch, Reason) ->
    ok.

%%------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
