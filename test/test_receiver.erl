-module(test_receiver).

-export([filterfn/1, handle_message/2]).

filterfn(_) -> true.

handle_message(_Msg, State) -> ok.
