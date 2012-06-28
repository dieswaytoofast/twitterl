%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Main module for the twitterl application.
%%% @end
%%%-------------------------------------------------------------------
-module(twitterl).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-include("defaults.hrl").

-export([start/0, stop/0]).
-export([get_env/0, get_env/1, get_env/2]).

-export([get_request_token/0, get_request_token/1, get_access_token/3]).
-export([update_status/3]).

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
    twitterl_requestor:get_request_token().

%% @doc Get a request token
-spec get_request_token(url()) -> #twitter_token_data{}.
get_request_token(URL) ->
    twitterl_requestor:get_request_token(URL).

%% @doc Get a request token
-spec get_access_token(token(), secret(), verifier()) -> #twitter_access_data{} | error().
get_access_token(Token, Secret, Verifier) ->
    twitterl_requestor:get_access_token(Token, Secret, Verifier).


%%% Status
%% @doc Update Status
-spec update_status(token(), secret(), status()) -> #tweet{} | error().
update_status(Token, Secret, Status) ->
    twitterl_requestor:update_status(Token, Secret, Status).
