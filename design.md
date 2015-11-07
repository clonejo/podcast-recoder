
http_feed_handler

http_media_file_handler

feeds: (called by http_feed_handler)
 - get_feed

feed_worker: (started by feeds)

media_files: (called by http_media_file_handler)
 - get_media_file

media_file_worker: (started by media_files)

When a file is requested from files and it has to be fetched, a worker will be started. While the causing request might time out early (1min), the fetching job will continue. For each file only one worker is started. Requests for a file that is fetched at the moment will just attach to the running worker.

Todo: (in order of importance)
 - required for first release:
   - delete old cached files adhering to a quota
   - add LICENSE
   - http_file_handler: honour if-not-modified
   - http_file_handler: use gzip compression
 - required for publicly hosting non-CC podcasts:
   - regularly request orig url for each feed/media file. delete cached version if 404.
 - use etags when fetching
 - http_file_handler: honour if-none-match (etag)
 - streaming, download, recode and send to the client at the same time
 - web page which lists cached versions of feeds and media files (show mnesia tables (feeds, media_files)
 - web page which lists download status of feeds/media files (data from feeds/media_files)
 - time jump handler: remove last_fetch, last_requested in file tables
