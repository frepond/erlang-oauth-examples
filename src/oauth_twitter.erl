%%
%% This is an example client for the Twitter API.
%%
%% Example usage:
%%
%%   $ make
%%   ...
%%   $ erl -pa ebin path/to/erlang-oauth/ebin -s crypto -s ssl -s inets
%%   ...
%%   1> Consumer = {"...KEY...", "...SECRET...", hmac_sha1}.
%%   ...
%%   2> {ok, Client} = oauth_twitter:start(Consumer).
%%   ...
%%   3> {ok, Token} = oauth_twitter:get_request_token(Client).
%%   ...
%%   4> AuthorizeURL = oauth_twitter:authorize_url(Token).
%%   ...
%%   5> ok = oauth_twitter:get_access_token(Client, "...VERIFIER (PIN)...").
%%   ...
%%   6> {ok, Headers, JSON} = oauth_twitter:get_favorites(Client).
%%   ...
%%
%% Alternativelly you coud use your app dev credetials:
%%
%%  $ erl -pa ebin path/to/erlang-oauth/ebin -s crypto -s ssl -s inets 
%%
%%   1> Consumer = {"...KEY...", "...SECRET...", hmac_sha1}.
%%   ...
%%   2> {ok, Client} = oauth_twitter:start(Consumer).
%%   ...
%%   3> ok = oauth_twitter:set_access_token(Client, "...Access Token...", "...Access Token Secret...", "...Owner...", "...Owner Id...").
%%   ...
%%   4> {ok, Headers, JSON} = oauth_twitter:get_favorites(Client).
%%   ...
%%
%% Note that before fetching the access token (step 5) you need to have
%% authorized the request token and been given the a verifier PIN at twitter.
%%
-module(oauth_twitter).
-export([ start/1
        , stop/1
        , get_request_token/1
        , post_request_token/2
        , authorize_url/1
        , get_access_token/2
        , post_access_token/2
        , verify_credentials/1
        , get_favorites/1
        , post_tweet/2
        , post_tweet/3
        , tweet_with_picture_filename/3
        , tweet_with_picture_filename/4
        , tweet_with_picture/4
        , tweet_with_picture/5
        , multipart_encode/5
        , get_friends/1
        , get_home_timeline/1
        , set_access_token/5
        ]).

-export_type([t/0]).
-type t() :: oauth_client:t().

%% Client logic
-spec start(oauth:consumer()) -> t().
start(Consumer) ->
  {ok, Client} = oauth_client:start(Consumer),
  Client.

-spec stop(t()) -> 'ok'.
stop(Client) ->
  oauth_client:stop(Client).

%% Verification
-spec get_request_token(t()) -> oauth_client:request_token_response().
get_request_token(Client) ->
  URL = "https://twitter.com/oauth/request_token",
  oauth_client:get_request_token(Client, URL).

-spec post_request_token(t(), nonempty_string()) -> oauth_client:request_token_response().
post_request_token(Client, RedirectUrl) ->
  URL = "https://twitter.com/oauth/request_token",
  Params = [ {"oauth_callback", RedirectUrl}],
  oauth_client:post_request_token(Client, URL, Params).

-spec set_access_token(t(), nonempty_string(), nonempty_string(), 
  nonempty_string(), nonempty_string()) -> oauth_client:request_token_response().
set_access_token(Client, AccessToken, AccessSecret, UserId, ScreenName) ->
  AParams = [
    {"oauth_token", AccessToken},
    {"oauth_token_secret", AccessSecret},
    {"user_id", UserId},
    {"screen_name", ScreenName}],
  oauth_client:set_access_token(Client, AParams).

% https://dev.twitter.com/docs/api/1/get/oauth/authorize
-spec authorize_url(string()) -> nonempty_string().
authorize_url(Token) ->
  oauth:uri("https://api.twitter.com/oauth/authorize", [{"oauth_token", Token}]).

-spec get_access_token(t(), string()) -> oauth_client:access_token_response().
get_access_token(Client, Verifier) ->
  URL = "https://twitter.com/oauth/access_token",
  oauth_client:get_access_token(Client, URL, [{"oauth_verifier", Verifier}]).

-spec post_access_token(t(), string()) -> oauth_client:access_token_response().
post_access_token(Client, Verifier) ->
  URL = "https://twitter.com/oauth/access_token",
  oauth_client:post_access_token(Client, URL, [{"oauth_verifier", Verifier}]).

%% Business Use cases

% https://dev.twitter.com/docs/api/1.1/get/account/verify_credentials
-spec verify_credentials(t()) -> oauth_client:http_response().
verify_credentials(Client) ->
  URL = "https://api.twitter.com/1.1/account/verify_credentials.json",
  oauth_client:get(Client, URL, []).

% https://dev.twitter.com/docs/api/1.1/get/favorites/list
-spec get_favorites(t()) -> oauth_client:http_response().
get_favorites(Client) ->
  URL = "https://api.twitter.com/1.1/favorites/list.json",
  oauth_client:get(Client, URL, []).

% https://dev.twitter.com/docs/api/1.1/post/statuses/update
-spec post_tweet(t(), nonempty_string()) -> oauth_client:http_response().
post_tweet(Client, Tweet) ->
  URL = "https://api.twitter.com/1.1/statuses/update.json",
  oauth_client:post(Client, URL, [{"status", Tweet}]).

-spec post_tweet(t(), nonempty_string(), binary()) -> oauth_client:http_response().
post_tweet(Client, Tweet, ReplyTweet) ->
  URL = "https://api.twitter.com/1.1/statuses/update.json",
  oauth_client:post(Client, URL, [{"status", Tweet},{"in_reply_to_status_id", ReplyTweet}]).

% https://dev.twitter.com/docs/api/1.1/post/statuses/update_with_media
% TODO: refactor this interface
-spec tweet_with_picture_filename(t(), nonempty_string(), file:filename()) -> oauth_client:http_response().
tweet_with_picture_filename(Client, Tweet, FileName) ->
  BaseN = filename:basename(FileName),
  {ok, File} = file:read_file(FileName),
  tweet_with_picture(Client, Tweet, File, BaseN).

-spec tweet_with_picture(t(), nonempty_string(), binary(), file:filename()) -> oauth_client:http_response().
tweet_with_picture(Client, Tweet, File, BaseN) ->
  URL   = "https://api.twitter.com/1.1/statuses/update_with_media.json",
  Bound = base64:encode_to_string(crypto:rand_bytes(32)),
  Msg   = multipart_encode(Bound, Tweet, File, BaseN, <<>>),
  oauth_client:post(Client
                   , URL
                   , lists:flatten(Msg)
                   , {multipart, Bound}).

-spec tweet_with_picture_filename(t(), nonempty_string(), file:filename(), binary()) -> oauth_client:http_response().
tweet_with_picture_filename(Client, Tweet, FileName, Reply) when is_binary(Reply) ->
  BaseN = filename:basename(FileName),
  {ok, File} = file:read_file(FileName),
  tweet_with_picture(Client, Tweet, File, BaseN, Reply).

-spec tweet_with_picture(t(), nonempty_string(), binary(), file:filename(), binary()) ->
                         oauth_client:http_response().
tweet_with_picture(Client, Tweet, File, BaseN, Reply) ->
  URL   = "https://api.twitter.com/1.1/statuses/update_with_media.json",
  Bound = base64:encode_to_string(crypto:rand_bytes(32)),
  Msg   = multipart_encode(Bound, Tweet, File, BaseN, Reply),
  %ok    = io:format("Msg:~n~s~n", [Msg]),
  oauth_client:post(Client
                   , URL
                   , lists:flatten(Msg)
                   , {multipart, Bound}).

% https://dev.twitter.com/docs/api/1.1/get/friends/ids
% aka as people you follow.
-spec get_friends(t()) -> oauth_client:http_response().
get_friends(Client) ->
  % Since we're using OAuth for authentication we end up no needing the
  % ScreenName and UserId arguments
  %Url = "https://api.twitter.com/1.1/friends/ids.json?screen_name=" ++ ScreenName ++ "&user_id=" ++ UserId,
  Url = "https://api.twitter.com/1.1/friends/ids.json",
  oauth_client:get(Client, Url, []).

% https://dev.twitter.com/docs/api/1.1/get/statuses/home_timeline
-spec get_home_timeline(t()) -> oauth_client:http_response().
get_home_timeline(Client) ->
  Url = "https://api.twitter.com/1.1/statuses/home_timeline.json",
  oauth_client:get(Client, Url, []).

%% Private
-spec multipart_encode(nonempty_string(), nonempty_string(), binary(), file:filename(), binary()) -> nonempty_string().
multipart_encode(Bound, Tweet, File, Basename, Reply) ->
  TweetPrefix = "Content-Disposition: form-data; name=\"status\"",
  FilePrefix1 = "Content-Type: application/octet-stream",
  FilePrefix2 = "Content-Disposition: form-data; name=\"media[]\"; filename=",
  ReplyLine   = case Reply of <<>>  -> ""; _ -> multipart_encode_reply(Bound, Reply) end,
  io_lib:format("--~s\r~n~s\r~n\r~n~s\r~n~s--~s\r~n~s\r~n~s\"~s\"\r~n\r~n~s\r~n--~s--\r~n"
    , [Bound, TweetPrefix, Tweet, ReplyLine, Bound, FilePrefix1, FilePrefix2, Basename, File, Bound]).

multipart_encode_reply(Bound, Reply) ->
  ReplyPrefix = "Content-Disposition: form-data; name=\"in_reply_to_status_id\"",
  io_lib:format("--~s\r~n~s\r~n\r~n~s\r~n", [Bound, ReplyPrefix, Reply]).
