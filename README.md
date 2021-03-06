
podcast_recoder recodes existing podcasts to smaller bitrates using the [Opus
Codec](http://opus-codec.org/). At 24kbit/s Opus delivers acceptable speech
quality for just 10MB per hour. Perfect for edge-land or rate-limiting
providers.

The *code structure* and a *list of todos* can be found in [DESIGN.md](DESIGN.md).

Installation
============
This application is still in development.

`$ rebar3 release`

For copyright reasons, you will likely want to have your instance not reachable
from the open internet. Consider placing a reverse-proxy server in front with
HTTP Authentication enabled or connecting through a (VPN) tunnel.

This program runs media files from the internet through ffmpeg. Beware of RCE
bugs.

Ansible playbooks can be found in `ansible/`. See [ansible/README.md](ansible/README.md).

Development
===========
An overview of the code can be found in `DESIGN.md`.

Dev Setup:
 * Install [rebar3](https://www.rebar3.org/).
 * Have Erlang installed (at least v18).
 * `$ run-dev.sh`
   * This will fetch all deps, compile, and run podcast_recoder.
     [Sync](https://github.com/rustyio/sync) will also be started.

Contact
=======
IRC: `#podrec` on Freenode
Mail: [clonejo@shakik.de](mailto:clonejo@shakik.de)

License (AGPL)
==============
Copyright 2015-2016 Feiko Nanninga

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
