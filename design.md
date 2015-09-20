
http_feed_handler

http_media_file_handler

feeds: (called by http_feed_handler)
 - get_feed

feed_worker: (started by feeds)

media_files: (called by http_media_file_handler)
 - get_media_file

media_file_worker: (started by media_files)


stuff:
 - regularly request orig url for each feed/media file. delete cached version if 404.
 - web page which lists cached versions of feeds and media files (show mnesia tables (feeds, media_files)
 - web page which lists download status of feeds/media files (data from feeds/media_files)
 - time jump handler: remove last_fetch in file tables
