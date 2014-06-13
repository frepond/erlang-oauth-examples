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

-compile(export_all).

start(Consumer) ->
  oauth_client:start(Consumer).

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

post_tweet(Client, Tweet) ->
  URL = "https://api.twitter.com/1.1/statuses/update.json",
  oauth_client:post(Client, URL, [{"status", Tweet}]).

%get_tweet(TweetId) ->
%  URL = "https://api.twitter.com/1.1/statuses/show/" 
%          ++ integer_to_list(TweetId)
%          ++ ".json",
%  oauth_client:get(Client, URL, []).

stop(Client) ->
  oauth_client:stop(Client).

get_friends(Client) -> %, ScreenName, UserId) ->
  % Since we're using OAuth for authentication we end up no needing the ScreenName and UserId
  %Url = "https://api.twitter.com/1.1/friends/ids.json?screen_name=" ++ ScreenName ++ "&user_id=" ++ UserId,
  Url = "https://api.twitter.com/1.1/friends/ids.json",
  %io:format("This is the URL~p~n", [Url]),
  oauth_client:get(Client, Url, []).
