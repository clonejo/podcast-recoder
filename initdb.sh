#!/bin/sh

rm Mnesia.nonode@nohost -rf &&  rebar3 compile && erl -pa _build/default/lib/*/ebin -s podcast_recoder_app initdb -s init stop
