
podcast_recoder aims to provide existing podcasts at smaller bitrates by using the [Opus Codec](http://opus-codec.org/). For example, an opus file at 24kbit/s has acceptable speech quality. 24kbit/s equals just 10MB per hour. Perfect for edge-land.

Installation
============
This application is still in development. When deemed stable enough, I'll provide releases (builds).

`$ rebar3 release`

Dev Setup
=========
 * Install [rebar3](https://www.rebar3.org/).
 * Have Erlang installed (v17 should suffice, this app uses maps).
 * `$ run-dev.sh`
   * This will fetch all deps, compile, and run podcast_recoder. [Sync](https://github.com/rustyio/sync) will also be started.

FAQ
===

_My feed doesn't work!_  
Yes, the XML parsing could be done better.
