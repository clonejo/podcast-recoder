General
=======

As the mechanism is very similar, feeds and attachments (enclosures) share a
lot of code (see 'generic' modules).

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
   attachments. gen_servers are started once for each variant.
   - `http_file_handler`: serves files through HTTP
   - `files`: manages jobs, keeps a queue to not overwhelm the CPU; behaviour
   - `file_worker`: started by `files`
   - `storage`: handles updates to the `*_cache` folder, gives out file descriptors
 - `feeds`: implements `files` behaviour
 - `attachments`: implements `files` behaviour
 - `util`
 - `records.hrl`: contains record for mnesia


Todo
====

(in order of importance)
 - required for first release:
   - delete old cached files adhering to a quota
   - http_file_handler: honour if-not-modified
   - http_file_handler: use gzip compression
 - required for publicly hosting non-CC podcasts:
   - regularly request orig url for each feed/attachment. delete cached version
     if 404.
 - use etags when fetching
 - http_file_handler: honour if-none-match (etag)
 - streaming, download, recode and send to the client at the same time
 - approximate file sizes in the feed
 - web page which lists cached versions of feeds and attachments (show mnesia
   tables (feeds, attachments)
 - web page which lists download status of feeds/attachments (data from
   feeds/attachments
 - time jump handler: remove last_fetch, last_requested in file tables
