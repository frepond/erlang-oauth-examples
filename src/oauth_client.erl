-module(oauth_client).

-behaviour(gen_server).
-include_lib("xmerl/include/xmerl.hrl").

-export([ access_token_params/1, deauthorize/1,
          get/2, get/3, get/4,
          post/2, post/3, post/4,
          get_access_token/2, get_access_token/3, get_access_token/4,
          get_request_token/2, get_request_token/3, get_request_token/4,
          post_request_token/2, post_request_token/3,
          post_access_token/2, post_access_token/3,
          start/1, start/2, start_link/1, start_link/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export_type([ t/0
             , request_token_response/0
             , access_token_response/0
             ]).
-opaque t() :: pid().

%%============================================================================
%% API functions
%%============================================================================
-spec start(oauth:consumer()) -> {'ok',t()}.
start(Consumer) ->
  gen_server:start(?MODULE, Consumer, []).

-type gen_server_name() :: {'global',_} 
                        | {'local',atom()} 
                        | {'via',atom() | tuple(),_}.
-type request_token_response()  :: {'ok', oauth:token()} | non_expected_results().
-type access_token_response()   :: 'ok' | non_expected_results().

-type get_parameter_method()    :: 'header'
                                |  'querystring'.
-type post_parameter_method()   :: get_parameter_method()
                                |  {'multipart', nonempty_string()}.
-type http_response()           :: {'ok', string(), string() | #xmlElement{}} 
                                |  non_expected_results().

-spec start(gen_server_name(),oauth:consumer()) -> {'ok',t()}.
start(ServerName, Consumer) ->
  gen_server:start(ServerName, ?MODULE, Consumer, []).

-spec start_link(oauth:consumer()) -> {'ok',t()}.
start_link(Consumer) ->
  gen_server:start_link(?MODULE, Consumer, []).

-spec start_link(gen_server_name(),oauth:consumer()) -> {'ok',t()}.
start_link(ServerName, Consumer) ->
  gen_server:start_link(ServerName, ?MODULE, Consumer, []).

-spec get_request_token(t(),oauth:url()) -> request_token_response().
get_request_token(Client, URL) ->
  get_request_token(Client, URL, [], header).

-spec get_request_token(t(),oauth:url(),oauth:params()) -> request_token_response().
get_request_token(Client, URL, Params) ->
  gen_server:call(Client, {get_request_token, URL, Params, header}).

-spec get_request_token(t(),oauth:url(),oauth:params(),get_parameter_method()) -> request_token_response().
get_request_token(Client, URL, Params, ParamsMethod) ->
  gen_server:call(Client, {get_request_token, URL, Params, ParamsMethod}).

-spec post_request_token(t(),oauth:url()) -> request_token_response().
post_request_token(Client, URL) ->
  gen_server:call(Client, {post_request_token, URL, [], header}).

-spec post_request_token(t(),oauth:url(),oauth:params()) -> request_token_response().
post_request_token(Client, URL, Params) ->
  gen_server:call(Client, {post_request_token, URL, Params, header}).

-spec get_access_token(t(),oauth:url()) -> access_token_response().
get_access_token(Client, URL) ->
  get_access_token(Client, URL, [], header).

-spec get_access_token(t(),oauth:url(),oauth:params()) -> access_token_response().
get_access_token(Client, URL, Params) ->
  gen_server:call(Client, {get_access_token, URL, Params, header}).

-spec get_access_token(t(),oauth:url(),oauth:params(),get_parameter_method()) -> access_token_response().
get_access_token(Client, URL, Params, ParamsMethod) ->
  gen_server:call(Client, {get_access_token, URL, Params, ParamsMethod}).

-spec post_access_token(t(),oauth:url()) -> access_token_response().
post_access_token(Client, URL) ->
  post_access_token(Client, URL, []).

-spec post_access_token(t(),oauth:url(),oauth:params()) -> access_token_response().
post_access_token(Client, URL, Params) ->
  gen_server:call(Client, {post_access_token, URL, Params, header}).

-spec get(t(),oauth:url()) -> http_response().
get(Client, URL) ->
  get(Client, URL, [], header).

-spec get(t(),oauth:url(),oauth:params()) -> http_response().
get(Client, URL, Params) ->
  gen_server:call(Client, {get, URL, Params, header}).

-spec get(t(),oauth:url(),oauth:params(),_) -> http_response().
get(Client, URL, Params, ParamsMethod) ->
  gen_server:call(Client, {get, URL, Params, ParamsMethod}).

-spec post(t(),oauth:url()) -> http_response().
post(Client, URL) ->
  post(Client, URL, [], header).

-spec post(t(),oauth:url(),oauth:params()) -> http_response().
post(Client, URL, Params) ->
  gen_server:call(Client, {post, URL, Params, header}).

-spec post(t(),oauth:url(),oauth:params(),_) -> http_response().
post(Client, URL, Params, ParamsMethod) ->
  gen_server:call(Client, {post, URL, Params, ParamsMethod}).

-spec access_token_params(t()) -> oauth:params().
access_token_params(Client) ->
  {ok, Params} = gen_server:call(Client, {access_token_params}),
  Params.

-spec deauthorize(t()) -> 'ok'.
deauthorize(Client) ->
  gen_server:cast(Client, deauthorize).

-spec stop(t()) -> 'ok'.
stop(Client) ->
  gen_server:cast(Client, stop).

%%============================================================================
%% Helper functions
%%============================================================================

-spec oauth_get(get_parameter_method(),oauth:url(),oauth:params(),oauth:consumer()
               ,oauth:token(),oauth:token_secret()) -> oauth:httpc_request_return().
oauth_get(header, URL, Params, Consumer, Token, TokenSecret) ->
  Signed = oauth:sign("GET", URL, Params, Consumer, Token, TokenSecret),
  {AuthorizationParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
  Request = {oauth:uri(URL, QueryParams), [oauth:header(AuthorizationParams)]},
  httpc:request(get, Request, [{autoredirect, false}], []);
oauth_get(querystring, URL, Params, Consumer, Token, TokenSecret) ->
  oauth:get(URL, Params, Consumer, Token, TokenSecret).

-spec oauth_post(post_parameter_method(),oauth:url(),oauth:params(),oauth:consumer()
                ,oauth:token(),oauth:token_secret()) -> oauth:httpc_request_return().
oauth_post(header, URL, Params, Consumer, Token, TokenSecret) ->
  Signed = oauth:sign("POST", URL, Params, Consumer, Token, TokenSecret),
  Request = {URL, [], "application/x-www-form-urlencoded", oauth:uri_params_encode(Signed)},
  httpc:request(post, Request, [], []);

oauth_post(querystring, URL, Params, Consumer, Token, TokenSecret) ->
  oauth:get(URL, Params, Consumer, Token, TokenSecret);

oauth_post({multipart, Boundary}, URL, Body, Consumer, Token, TokenSecret) ->
  BodyH   = base64:encode_to_string(crypto:hash(sha, Body)),
  Signed  = oauth:sign("POST", URL, [{"oauth_body_hash", BodyH}]
                      , Consumer, Token, TokenSecret),
  {AuthorizationParams, []} =
            lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
  Headers = [ oauth:header(AuthorizationParams)],
  ContentType = "multipart/form-data;boundary=" ++ Boundary,
  Request = {URL, Headers, ContentType, Body},
  httpc:request(post, Request, [], []).

%%============================================================================
%% gen_server callbacks
%%============================================================================

-type state() :: {oauth:consumer()}
              |  {oauth:consumer(), oauth:params()}
              |  {oauth:consumer(), oauth:params(), oauth:params()}.
-spec init(oauth:consumer()) -> {'ok',state()}.
init(Consumer) ->
  {ok, {Consumer}}.

-type param_call_methods() :: 'get' | 'get_access_token' | 'get_request_token'
                            | 'post' | 'post_access_token' | 'post_request_token'.
-type call_methods() :: {'access_token_params'} 
                      | { param_call_methods(), oauth:url(), oauth:params()
                          , oauth:consumer(), post_parameter_method()}.

-type non_expected_results() :: {'non_200', oauth:httpc_request_return()}
                             |  {'error', term()}.
-type response() :: 'ok'
                 |  {'ok', nonempty_string()}
                 |  {'ok', string(), string() | #xmlElement{}} 
                 |  {'ok', oauth:params()}
                 |  non_expected_results().
-spec handle_call(call_methods(), _, state()) -> {'reply',response(),state()}.

handle_call({get_request_token, URL, Params, ParamsMethod}, _From, State={Consumer}) ->
  case oauth_get(ParamsMethod, URL, Params, Consumer, "", "") of
    {ok, Response={{_, 200, _}, _, _}} ->
      RParams = oauth:params_decode(Response),
      Token   = oauth:token(RParams),
      {reply, {ok, Token}, {Consumer, RParams}};
    {ok, Response} ->
      {reply, {non_200, Response}, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({post_request_token, URL, Params, ParamsMethod}, _From, State={Consumer}) ->
  case oauth_post(ParamsMethod, URL, Params, Consumer, "", "") of
    {ok, Response={{_, 200, _}, _, _}} ->
      RParams = oauth:params_decode(Response),
      Token   = oauth:token(RParams),
      {reply, {ok, Token}, {Consumer, RParams}};
    {ok, Response} ->
      {reply, {non_200, Response}, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({get_access_token, URL, Params, ParamsMethod}, _From, State={Consumer, RParams}) ->
  case oauth_get(ParamsMethod, URL, Params, Consumer, oauth:token(RParams), oauth:token_secret(RParams)) of
    {ok, Response={{_, 200, _}, _, _}} ->
      AParams = oauth:params_decode(Response),
      {reply, ok, {Consumer, RParams, AParams}};
    {ok, Response} ->
      {reply, {non_200, Response}, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({post_access_token, URL, Params, ParamsMethod}, _From, State={Consumer, RParams}) ->
  case oauth_post(ParamsMethod, URL, Params, Consumer, oauth:token(RParams), oauth:token_secret(RParams)) of
    {ok, Response={{_, 200, _}, _, _}} ->
      AParams = oauth:params_decode(Response),
      %AccessToken   = oauth:token(RParams),
      %AccessSecret  = oauth:token_secret(RParams),
      {reply, ok, {Consumer, RParams, AParams}};
    {ok, Response} ->
      {reply, {non_200, Response}, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({get, URL, Params, ParamsMethod}, _From, State={Consumer, _RParams, AParams}) ->
  case oauth_get(ParamsMethod, URL, Params, Consumer, oauth:token(AParams), oauth:token_secret(AParams)) of
    {ok, {{_, 200, _}, Headers, Body}} ->
      case proplists:get_value("content-type", Headers) of
        undefined ->
          {reply, {ok, Headers, Body}, State};
        ContentType ->
          MediaType = hd(string:tokens(ContentType, ";")),
          case lists:suffix("/xml", MediaType) orelse lists:suffix("+xml", MediaType) of
            true ->
              {XML, []} = xmerl_scan:string(Body),
              {reply, {ok, Headers, XML}, State};
            false ->
              {reply, {ok, Headers, Body}, State}
          end
      end;
    {ok, Response} ->
      {reply, {non_200, Response}, State};
    Error ->
      {reply, Error, State}
  end;
handle_call({post, URL, Params, ParamsMethod}, _From, State={Consumer, _RParams, AParams}) ->
  case oauth_post(ParamsMethod, URL, Params, Consumer, oauth:token(AParams), oauth:token_secret(AParams)) of
    {ok, {{_, 200, _}, Headers, Body}} ->
      case proplists:get_value("content-type", Headers) of
        undefined ->
          {reply, {ok, Headers, Body}, State};
        ContentType ->
          MediaType = hd(string:tokens(ContentType, ";")),
          case lists:suffix("/xml", MediaType) orelse lists:suffix("+xml", MediaType) of
            true ->
              {XML, []} = xmerl_scan:string(Body),
              {reply, {ok, Headers, XML}, State};
            false ->
              {reply, {ok, Headers, Body}, State}
          end
      end;
    {ok, Response} ->
      {reply, {non_200, Response}, State};
    Error ->
      {reply, Error, State}
  end;

handle_call({access_token_params}, _From, State={_Consumer, _RParams, AParams}) ->
  {reply, {ok, AParams}, State}.

-spec handle_cast('deauthorize' | 'stop',state()) -> 
                  {'noreply',state()} | {'stop','normal',state()}.
handle_cast(deauthorize, {Consumer, _RParams}) ->
  {noreply, {Consumer}};
handle_cast(deauthorize, {Consumer, _RParams, _AParams}) ->
  {noreply, {Consumer}};
handle_cast(stop, State) ->
  {stop, normal, State}.

-spec handle_info(_,_) -> {'noreply',_}.
handle_info(_Msg, State) ->
  {noreply, State}.

-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec terminate('normal',_) -> 'ok'.
terminate(normal, _State) ->
  ok.
