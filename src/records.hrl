% Copyright 2015 Feiko Nanninga
% This file is part of podcast_recoder, a project licensed under the terms of
% the GNU Affero General Public License Version 3 (see LICENSE).

% mnesia tables
-record(file, {local_name, % used for URL and filename
               orig_url,
               last_fetch, % timestamp when the file has last been fetched (by us)
               last_requested % timestamp when the file has last been requested (by a user),
                              % used for cache housekeeping
              }).

