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
-export([setup/0]).

-define(APP, ?MODULE).


%% @doc Start the application and all its dependencies.
-spec start() -> ok.
start() ->
    application:start(sasl),
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    application:start(ejson),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),      %% required by oauth
    application:start(oauth),
    application:start(gproc),
    application:start(twitterl).


%% @doc Stop the application and all its dependencies.
-spec stop() -> ok.
stop() ->
    application:stop(twitterl),
    application:stop(gproc),
    application:stop(oauth),
    application:stop(inets),       %% required by oauth
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    application:stop(ejson),
    application:stop(lager),
    application:stop(syntax_tools),
    application:stop(compiler),
    application:stop(sasl).


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
