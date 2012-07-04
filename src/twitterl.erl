%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Main module for the twitterl application.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(twitterl).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-include("defaults.hrl").

-export([start/0, stop/0]).
-export([get_env/0, get_env/1, get_env/2]).

-export([get_request_token/0, get_request_token/1, get_access_token/3]).
-export([statuses_home_timeline/4, 
         statuses_user_timeline/4,
         statuses_retweeted_by_me/4,
         statuses_retweeted_to_me/4,
         statuses_retweets_of_me/4,
         statuses_retweeted_to_user/4,
         statuses_retweeted_by_user/4,
         statuses_mentions/4]).
-export([status_update/3]).

-export([setup/0]).

-define(APP, ?MODULE).


%% @doc Start the application and all its dependencies.
-spec start() -> ok.
start() ->
    start_deps(?APP).

-spec start_deps(App :: atom()) -> ok.
start_deps(App) ->
    application:load(App),
    {ok, Deps} = application:get_key(App, applications),
    lists:foreach(fun start_deps/1, Deps),
    start_app(App).

-spec start_app(App :: atom()) -> ok.
start_app(App) ->
    case application:start(App) of
        {error, {already_started, _}} -> ok;
        ok                            -> ok
    end.


%% @doc Stop the application and all its dependencies.
-spec stop() -> ok.
stop() ->
    stop_deps(?APP).

-spec stop_deps(App :: atom()) -> ok.
stop_deps(App) ->
    stop_app(App),
    {ok, Deps} = application:get_key(App, applications),
    lists:foreach(fun stop_deps/1, lists:reverse(Deps)).

-spec stop_app(App :: atom()) -> ok.
stop_app(kernel) ->
    ok;
stop_app(stdlib) ->
    ok;
stop_app(App) ->
    case application:stop(App) of
        {error, {not_started, _}} -> ok;
        ok                        -> ok
    end.


%% @doc Retrieve all key/value pairs in the env for the specified app.
-spec get_env() -> [{Key :: atom(), Value :: term()}].
get_env() ->
    application:get_all_env(?APP).

%% @doc The official way to get a value from the app's env.
%%      Will return the 'undefined' atom if that key is unset.
-spec get_env(Key :: atom()) -> term().
get_env(Key) ->
    get_env(Key, undefined).

%% @doc The official way to get a value from this application's env.
%%      Will return Default if that key is unset.
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    case application:get_env(?APP, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

%% @doc setup that needs to be done to get the system running the *first* time
%%      on this node
-spec setup() -> ok | error().
setup() ->
    ok.

%%% Authorization
%% @doc Get a request token
-spec get_request_token() -> #twitter_token_data{}.
get_request_token() ->
    get_request_token(?TWITTERL_CALLBACK_URL).

%% @doc Get a request token
-spec get_request_token(url()) -> #twitter_token_data{}.
get_request_token(URL) ->
    twitterl_requestor:get_request_token(URL).

%% @doc Get a request token
-spec get_access_token(verifier(), token(), secret()) -> #twitter_access_data{} | error().
get_access_token(Verifier, Token, Secret) ->
    twitterl_requestor:get_access_token(Verifier, Token, Secret).

%%% Timelines
%% @doc get the home timeline
-spec statuses_home_timeline(target(), params(), token(), secret()) -> {ok, request_reference()} | error().
statuses_home_timeline(Target, Params, Token, Secret) ->
    {RequestType, HttpRequestType, URL} = ?TWITTER_HOME_TIMELINE,
    twitterl_requestor:process_request(Target, RequestType, HttpRequestType, URL, Params, Token, Secret).

-spec statuses_user_timeline(target(), params(), token(), secret()) -> {ok, request_reference()} | error().
statuses_user_timeline(Target, Params, Token, Secret) ->
    {RequestType, HttpRequestType, URL} = ?TWITTER_USER_TIMELINE,
    twitterl_requestor:process_request(Target, RequestType, HttpRequestType, URL, Params, Token, Secret).

-spec statuses_mentions(target(), params(), token(), secret()) -> {ok, request_reference()} | error().
statuses_mentions(Target, Params, Token, Secret) ->
    {RequestType, HttpRequestType, URL} = ?TWITTER_MENTIONS,
    twitterl_requestor:process_request(Target, RequestType, HttpRequestType, URL, Params, Token, Secret).

-spec statuses_retweeted_by_me(target(), params(), token(), secret()) -> {ok, request_reference()} | error().
statuses_retweeted_by_me(Target, Params, Token, Secret) ->
    {RequestType, HttpRequestType, URL} = ?TWITTER_RETWEETED_BY_ME,
    twitterl_requestor:process_request(Target, RequestType, HttpRequestType, URL, Params, Token, Secret).

-spec statuses_retweeted_to_me(target(), params(), token(), secret()) -> {ok, request_reference()} | error().
statuses_retweeted_to_me(Target, Params, Token, Secret) ->
    {RequestType, HttpRequestType, URL} = ?TWITTER_RETWEETED_TO_ME,
    twitterl_requestor:process_request(Target, RequestType, HttpRequestType, URL, Params, Token, Secret).

-spec statuses_retweets_of_me(target(), params(), token(), secret()) -> {ok, request_reference()} | error().
statuses_retweets_of_me(Target, Params, Token, Secret) ->
    {RequestType, HttpRequestType, URL} = ?TWITTER_RETWEETS_OF_ME,
    twitterl_requestor:process_request(Target, RequestType, HttpRequestType, URL, Params, Token, Secret).

-spec statuses_retweeted_to_user(target(), params(), token(), secret()) -> {ok, request_reference()} | error().
statuses_retweeted_to_user(Target, Params, Token, Secret) ->
    {RequestType, HttpRequestType, URL} = ?TWITTER_RETWEETED_TO_USER,
    twitterl_requestor:process_request(Target, RequestType, HttpRequestType, URL, Params, Token, Secret).

-spec statuses_retweeted_by_user(target(), params(), token(), secret()) -> {ok, request_reference()} | error().
statuses_retweeted_by_user(Target, Params, Token, Secret) ->
    {RequestType, HttpRequestType, URL} = ?TWITTER_RETWEETED_BY_USER,
    twitterl_requestor:process_request(Target, RequestType, HttpRequestType, URL, Params, Token, Secret).



%%% Status
%% @doc Update Status
-spec status_update(status(), token(), secret()) -> #tweet{} | error().
status_update(Status, Token, Secret) ->
    twitterl_requestor:status_update(Status, Token, Secret).
