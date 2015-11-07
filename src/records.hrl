
-record(file, {local_name, % used for URL and filename
               orig_url,
               last_fetch, % timestamp when the file has last been fetched (by us)
               last_requested % timestamp when the file has last been requested (by a user),
                              % used for cache housekeeping
              }).

