-module(podrec_file_type).

-callback mnesia_table_name() -> TableName :: atom().

-callback get_cached_file_path(LocalName :: binary()) -> Path :: term().

-callback try_recode(OriginalFilePath :: term()) -> {finished, RecodedFilePath :: term()} | {error, Reason :: term()}.

-callback file_fetch_user_timeout() -> Milliseconds :: integer().

-callback file_recent() -> Seconds :: integer().

-callback get_file_original_url(LocalName :: binary()) -> {ok, Path :: term()} | {error, Reason :: term()}.
