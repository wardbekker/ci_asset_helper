#!/bin/sh

rebar compile skip_deps=true && erl -mnesia dir db_data -pa `pwd`/ebin `pwd`/deps/*/ebin -boot start_sasl +c +P 134217727 -run reloader
