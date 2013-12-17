%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Ruan Pienaar <ruan.pienaar@erlang-solutions.com>
%%% @doc 
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(ofs_handler_app).
-copyright("2013, Erlang Solutions Ltd.").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case ofs_handler_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
		end.

stop(_State) ->
    ok.