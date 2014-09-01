%%
%% This is an example client for the Twitter API.
%%
%% Example usage:
%%
%%   $ make
%%   ...
%%   $ erl -pa ebin -pa path/to/erlang-oauth/ebin -s crypto -s ssl -s inets
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
%% Note that before fetching the access token (step 5) you need to have
%% authorized the request token and been given the a verifier PIN at twitter.
%%
-module(oauth_twitter).
-export([ start/1
        , get_request_token/1
        , authorize_url/1
        , get_access_token/2
        , get_favorites/1
        , verify_credentials/1
        , post_request_token/2
        , post_access_token/2
        , post_tweet_foo/2
        , tweet_with_picture/3
        , tweet_with_picture/4
        , stop/1
        , get_friends/1
        , get_home_timeline/1
        ]).

-export_type([t/0]).
-opaque t() :: pid().

-spec start({string(), string()}) -> t().
start(Consumer) ->
  {ok, Client} = oauth_client:start(Consumer),
  Client.

get_request_token(Client) ->
  URL = "https://twitter.com/oauth/request_token",
  oauth_client:get_request_token(Client, URL).

authorize_url(Token) ->
  oauth:uri("https://twitter.com/oauth/authorize", [{"oauth_token", Token}]).

get_access_token(Client, Verifier) ->
  URL = "https://twitter.com/oauth/access_token",
  oauth_client:get_access_token(Client, URL, [{"oauth_verifier", Verifier}]).

get_favorites(Client) ->
  URL = "https://api.twitter.com/1.1/favorites/list.json",
  oauth_client:get(Client, URL, []).

verify_credentials(Client) ->
  URL = "https://api.twitter.com/1.1/account/verify_credentials.json",
  oauth_client:get(Client, URL, []).

%% V2.0
post_request_token(Client, RedirectUrl) ->
  URL = "https://twitter.com/oauth/request_token",
  Params = [ {"oauth_callback", RedirectUrl}],
  oauth_client:post_request_token(Client, URL, Params).

post_access_token(Client, Verifier) ->
  URL = "https://twitter.com/oauth/access_token",
  oauth_client:post_access_token(Client, URL, [{"oauth_verifier", Verifier}]).

post_tweet_foo(Client, Tweet) ->
  URL = "https://api.twitter.com/1.1/statuses/update.json",
  oauth_client:post(Client, URL, [{"status", Tweet}]).

tweet_with_picture(Client, Tweet, FileName) ->
  BaseN = filename:basename(FileName),
  {ok, File} = file:read_file(FileName),
  tweet_with_picture(Client, Tweet, File, BaseN).

tweet_with_picture(Client, Tweet, File, BaseN) ->
  URL   = "https://api.twitter.com/1.1/statuses/update_with_media.json",
  Bound = base64:encode_to_string(crypto:rand_bytes(32)),
  Msg   = io_lib:format("--~s\r~nContent-Disposition: form-data; name=\"status\"\r~n\r~n~s\r~n--~s\r~nContent-Type: application/octet-stream\r~nContent-Disposition: form-data; name=\"media[]\"; filename=\"~s\"\r~n\r~n~s\r~n--~s--\r~n"
    , [Bound, Tweet, Bound, BaseN, File, Bound]),
  oauth_client:post(Client
                   , URL
                   , lists:flatten(Msg)
                   , {multipart, Bound}).

stop(Client) ->
  oauth_client:stop(Client).

get_friends(Client) -> %, ScreenName, UserId) ->
  % Since we're using OAuth for authentication we end up no needing the ScreenName and UserId
  %Url = "https://api.twitter.com/1.1/friends/ids.json?screen_name=" ++ ScreenName ++ "&user_id=" ++ UserId,
  Url = "https://api.twitter.com/1.1/friends/ids.json",
  oauth_client:get(Client, Url, []).

get_home_timeline(Client) ->
  Url = "https://api.twitter.com/1.1/statuses/home_timeline.json",
  oauth_client:get(Client, Url, []).
