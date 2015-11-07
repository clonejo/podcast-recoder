
podcast_recoder recodes existing podcasts to smaller bitrates using the [Opus
Codec](http://opus-codec.org/). At 24kbit/s Opus delivers acceptable speech
quality for just 10MB per hour. Perfect for edge-land or rate-limiting
providers.

Installation
============
This application is still in development. When deemed stable enough, I'll
provide releases (builds).

`$ rebar3 release`

Development
===========

An overview of the code can be found in `DESIGN.md`.

Dev Setup:
 * Install [rebar3](https://www.rebar3.org/).
 * Have Erlang installed (at least v17).
 * `$ run-dev.sh`
   * This will fetch all deps, compile, and run podcast_recoder.
     [Sync](https://github.com/rustyio/sync) will also be started.

FAQ
===

_My feed doesn't work!_  
Yes, the XML parsing could be done better.
