
The `podcast_recoder.yml` playbook installs the application on a single-purpose Debian 8 (Jessie) machine. After that, run `initdb.yml` to set up the database.

If you had Erlang from the Debian repos installed before, run `apt-get dist-upgrade`.

You can set a HTTP auth password in your host variables (otherwise the server will be available to anyone!):
    hostname http_auth_user="foo" http_auth_password="bar"

Configure feeds and other settings in `roles/podcast_recoder/files/sys.config`.
