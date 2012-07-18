%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @end
%%%-------------------------------------------------------------------
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
%%
%% Errors
%%
-define(AUTH_ERROR, authentication_error).
-define(DEAD_PROCESS, dead_process).
-define(RATE_ERROR, rate_error).
-define(INVALID_REQUEST_TYPE, invalid_request_type).
-define(EMPTY_ERROR, empty).
-define(INVALID_BOOLEAN, invalid_boolean).
-define(INVALID_STRING, invalid_string).
-define(INVALID_BINARY, invalid_binary).
-define(INVALID_INTEGER, invalid_integer).

%% Twitter
-define(TWITTERL_PROCESSOR_SUP, twitterl_processor_sup).
-define(TWITTERL_PROCESSOR, twitterl_processor).
-define(TWITTERL_PARSER_SUP, twitterl_parser_sup).
-define(TWITTERL_PARSER, twitterl_parser).
-define(TWITTERL_REQUESTOR_SUP, twitterl_requestor_sup).
-define(TWITTERL_REQUESTOR, twitterl_requestor).
-define(TWITTERL_REST_PROCESSOR, twitterl_rest_processor).
-define(TWITTERL_STREAM_PROCESSOR, twitterl_stream_processor).
-define(TWITTERL_REQUEST_TYPE_REST, rest).
-define(TWITTERL_REQUEST_TYPE_STREAM, stream).
-define(TWITTERL_HTTP_REQUEST_TYPE_GET, get).
-define(TWITTERL_HTTP_REQUEST_TYPE_POST, post).
-define(TWITTERL_ITEM_TYPE_TWEET, tweet).
-define(TWITTERL_ITEM_TYPE_USER, user).
-define(TWITTERL_PARSER_COUNT, 2).
-define(TWITTERL_REQUESTOR_COUNT, 2).
-define(TWITTERL_RETRY_COUNT, 5).

%% Gproc
-define(GPROC_REGISTRATION_ERROR, gproc_registration_error).
-define(GPROC_UNKNOWN_PROCESS, gproc_unknown_process).

%% Timer
-define(DEFAULT_TIMER_TIMEOUT, 5000).
%% Processor
-define(REQUEST_COMPLETED, request_completed).

%% Twitter API URLs
-define(TWITTER_FAVORITES, {rest, get, "https://api.twitter.com/1/favorites.json"}).
-define(TWITTER_HOME_TIMELINE, {rest, get, "https://api.twitter.com/1/statuses/home_timeline.json"}).
-define(TWITTER_USER_TIMELINE, {rest, get, "https://api.twitter.com/1/statuses/user_timeline.json"}).
-define(TWITTER_USER_TIMELINE_STREAM, {stream, post, "https://userstream.twitter.com/2/user.json"}).
-define(TWITTER_MENTIONS, {rest, get, "https://api.twitter.com/1/statuses/mentions.json"}).
-define(TWITTER_RETWEETED_BY_ME, {rest, get, "https://api.twitter.com/1/statuses/retweeted_by_me.json"}).
-define(TWITTER_RETWEETED_TO_ME, {rest, get, "https://api.twitter.com/1/statuses/retweeted_to_me.json"}).
-define(TWITTER_RETWEETS_OF_ME, {rest, get, "https://api.twitter.com/1/statuses/retweets_of_me.json"}).
-define(TWITTER_RETWEETED_TO_USER, {rest, get, "https://api.twitter.com/1/statuses/retweeted_to_user.json"}).
-define(TWITTER_RETWEETED_BY_USER, {rest, get, "https://api.twitter.com/1/statuses/retweeted_by_user.json"}).
-define(TWITTER_REQUEST_TOKEN, {rest, get, "https://api.twitter.com/oauth/request_token"}).
-define(TWITTER_ACCESS_TOKEN, {rest, post, "https://api.twitter.com/oauth/access_token"}).
-define(TWITTER_STATUS_UPDATE, {rest, post, "https://api.twitter.com/1/statuses/update.json"}).
-define(TWITTER_STATUS_UPDATE_WITH_MEDIA, {rest, post, "https://api.twitter.com/1/statuses/update_with_media.json"}).
-define(TWITTER_STATUS_SHOW, {rest, get, "https://api.twitter.com/1/statuses/show.json"}).
-define(TWITTER_STATUS_DESTROY, {rest, post, "https://api.twitter.com/1/statuses/destroy"}).
-define(TWITTER_STATUS_RETWEETED_BY, {rest, post, "https://api.twitter.com/1/statuses"}).
-define(TWITTER_STATUS_RETWEETS, {rest, post, "https://api.twitter.com/1/statuses/retweets"}).
-define(TWITTER_STATUS_RETWEET, {rest, post, "https://api.twitter.com/1/statuses/retweet"}).
-define(TWITTER_STATUS_OEMBED, {rest, post, "https://api.twitter.com/1/statuses/oembed"}).
-define(TWITTER_USERS_SHOW, {rest, get, "https://api.twitter.com/1/users/show.json"}).
-define(TWITTER_USERS_LOOKUP, {rest, post, "https://api.twitter.com/1/users/lookup.json"}).
-define(TWITTER_ACCOUNT_VERIFY_CREDENTIALS, {rest, get, "https://api.twitter.com/1/account/verify_credentials.json"}).
-define(TWITTER_ACCOUNT_SETTINGS, {rest, get, "https://api.twitter.com/1/account/settings.json"}).

%% Callback URL
-define(TWITTERL_CALLBACK_URL, "http://www.posttestserver.com").


%%
%% Types
%%
-type error()                                 :: {error, Reason :: term()}.
-type request_reference()                     :: {pid(), pid()}.
-type request_type()                          :: stream|rest.
-type http_request_type()                     :: get|post.
-type process_name()                          :: request_type().
-type target()                                :: {atom(), pid() | atom() | fun()}.
-type user_id()                               :: integer().
-type request_id()                            :: {pid(), pid()}.
-type status_id()                             :: binary().
-type token()                                 :: binary().
-type secret()                                :: binary().
-type status()                                :: binary().
-type verifier()                              :: binary().
-type url()                                   :: binary().
-type method()                                :: atom().
-type string_method()                         :: string().
-type params()                                :: list().
-type consumer()                              :: {string(), string(), atom()}.
-type item_type()                           :: tweet | user.

-include("../include/twitterl.hrl").

