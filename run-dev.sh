#!/bin/sh

rebar3 as dev compile && erl -pa _build/dev/lib/*/ebin -config sys.config -boot start_sasl -s podcast_recoder_app -s sync go
