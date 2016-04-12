General
=======

As the mechanism is very similar, feeds and enclosures share a lot of code (see
'generic' modules).

When a file is requested from files and it has to be fetched, a worker will be
started. While the causing request might time out early (1min), the fetching
job will continue. For each file only one worker is started. Requests for a
file that is fetched at the moment will just attach to the running worker.

'Fetching' is considered to contain downloading, recoding and storing a file.

Mnesia is used to store (on disk) which local filename refers to which url and
some metadata for caching.


Modules
=======

 - `app`: sets up cowboy and starts root supervisor
 - `sup`: the root supervisor
 - generic: These modules are parameterized and used for both feeds and
   enclosures. gen_servers are started once for each variant.
   - `http_file_handler`: serves files through HTTP
   - `files`: manages jobs, keeps a queue to not overwhelm the CPU; behaviour
   - `file_worker`: started by `files`
   - `storage`: handles updates to the `*_cache` folder, gives out file descriptors
 - `feeds`: implements `files` behaviour
 - `enclosures`: implements `files` behaviour
 - `util`
 - `records.hrl`: contains record for mnesia


Todo
====

(in order of importance)
 - required for first release:
   - delete old cached files adhering to a quota
   - http_file_handler: honour if-not-modified
   - http_file_handler: use gzip compression
   - streaming, download, recode and send to the client at the same time
 - required for publicly hosting podcasts:
   - to check for modifications, do a HEAD request without creating a fetch job
   - on HEAD request delete cached version if 404.
 - approximate file sizes in the feed
 - web page which lists cached versions of feeds and enclosures (show mnesia
   tables (feeds, enclosures)
 - web page which lists download status of feeds/enclosures (data from
   feeds/enclosures
 - time jump handler: remove last_fetch, last_requested in file tables
 - use etags when fetching
 - http_file_handler: honour if-none-match (etag)
