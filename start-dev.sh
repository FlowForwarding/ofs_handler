#!/bin/sh
exec erl -pa deps/*/ebin -pa ebin \
    -boot start_sasl \
    -config system.config \
    -sname local_ofs_handler \
    -init_debug