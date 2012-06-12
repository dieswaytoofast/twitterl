%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Module serving twitterl_receiver functions
%%% @end
%%%-------------------------------------------------------------------
-module(twitterl_receiver).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([set_max_id/1, set_since_id/1]).
-export([get_state/0]).
-export([get_request/1, get_request/2, process_url/3, process_url/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").
-include("twitterl_receiver.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Set the max_id for the twitter stream
-spec set_max_id(MaxId::integer()) -> {ok, MaxId::integer()} | error().
set_max_id(MaxId) when is_integer(MaxId) ->
    TwitterlReceiver = {?TWITTERL_RECEIVER, ?TWITTERL_RECEIVER},
    twitterl_manager:safe_call(TwitterlReceiver, {set_max_id, MaxId}).

%% @doc Set the since_id for the twitter stream
-spec set_since_id(SinceId::integer()) -> {ok, SinceId::integer()} | error().
set_since_id(SinceId) when is_integer(SinceId) ->
    TwitterlReceiver = {?TWITTERL_RECEIVER, ?TWITTERL_RECEIVER},
    twitterl_manager:safe_call(TwitterlReceiver, {set_since_id, SinceId}).

%% @doc Get the state for the receiver
-spec get_state() -> {ok, #receiver_state{}} | error().
get_state() ->
    TwitterlReceiver = {?TWITTERL_RECEIVER, ?TWITTERL_RECEIVER},
    twitterl_manager:safe_call(TwitterlReceiver, {get_state}).

%% @doc get the http Request associated with the URL
-spec get_request(URL::string()) -> [{string(), string()}].
get_request(URL) ->
    get_request(URL, []).

%% @doc get the http Request associated with the URL and Params
-spec get_request(URL::string(), Params::list()) -> [{string(), string()}].
get_request(URL, Params) ->
    TwitterlReceiver = {?TWITTERL_RECEIVER, ?TWITTERL_RECEIVER},
    twitterl_manager:safe_call(TwitterlReceiver, {request, URL, Params}).

%% @doc Run the request on the URL in stream or REST mode. The result will be
%%          sent to ReceiverPid
-spec process_url(ReceiverPid::pid(), URL::string(), Type::rest|stream) -> any().
process_url(ReceiverPid, Type, URL) -> 
    process_url(ReceiverPid, Type, URL, []).

%% @doc Run the request on the URL and Params in stream or REST mode. The result will be
%%          sent to ReceiverPid
-spec process_url(ReceiverPid::pid(), Type::rest|stream, URL::string(), Params::list()) -> any().
process_url(ReceiverPid, Type, URL, Params) -> 
    Request = get_request(URL, Params),
    case Type of 
        stream ->
            _ProcessorPid = twitterl_processor_sup:start_processor(ReceiverPid, Request);
        _ ->
		    case httpc:request(get, Request, [{autoredirect, false}], []) of
		        {ok, {_Result, _Headers, BinBody}} ->
		            try 
		                JsonBody= ejson:decode(BinBody),
		                lager:debug("returning data to:~p, JsonBody:~p~n, BinBody:~p~n", [ReceiverPid, JsonBody, BinBody]),
		                ReceiverPid ! {data, JsonBody} 
		            catch
		                Class:Reason ->
		                    lager:debug("catch send_data Class:~p~n, Reason:~p~n", [Class, Reason]) 
		            end;
		        _Other ->
		            lager:debug("Rest Request:~p~n failed with Reason:~p~n", [Request, _Other])
		    end
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).


init(_Args) ->
    twitterl_manager:register_process(?TWITTERL_RECEIVER, ?TWITTERL_RECEIVER),
    OAuthData = load_tokens(),
    State = #receiver_state{
            oauth_data = OAuthData},
    {ok, State}.


handle_call({request, URL, Params}, _From, State) ->
    OAuthData = State#receiver_state.oauth_data,
    Request = create_request(OAuthData, URL, Params),
    {reply, Request, State};

handle_call({set_max_id, MaxId}, _From, State) ->
    NewState = State#receiver_state{max_id = MaxId},
    {reply, {ok, MaxId}, NewState};

handle_call({set_since_id, SinceId}, _From, State) ->
    NewState = State#receiver_state{since_id = SinceId},
    {reply, {ok, SinceId}, NewState};

handle_call({get_state}, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    lager:debug("3, ~p~n", [_Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    lager:debug("Message:~p~n~n~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc Get the OAuth credentials for the account
-spec load_tokens() -> #twitter_oauth_data{}.
load_tokens() ->
    #twitter_oauth_data{
        consumer_key = twitterl:get_env(oauth_consumer_key),
        consumer_secret = twitterl:get_env(oauth_consumer_secret),
        access_token = twitterl:get_env(oauth_access_token),
        access_token_secret = twitterl:get_env(oauth_access_token_secret)
        }.

%% @doc Get the consumer credentials for the account
-spec get_consumer(#twitter_oauth_data{}) -> {string(), string(), atom()}.
get_consumer(OAuthData) ->
    {OAuthData#twitter_oauth_data.consumer_key,
     OAuthData#twitter_oauth_data.consumer_secret,
     hmac_sha1}.


%% @doc Sign the Request
-spec sign_request(#twitter_oauth_data{}, string(), list()) -> [{string(), string()}].
sign_request(OAuthData, URL, Params) -> 
    Consumer = get_consumer(OAuthData),
    AccessToken = OAuthData#twitter_oauth_data.access_token, 
    AccessTokenSecret = OAuthData#twitter_oauth_data.access_token_secret, 
    oauth:sign("GET", URL, Params, Consumer, AccessToken, AccessTokenSecret).

%% @doc Create the Request
-spec create_request(#twitter_oauth_data{}, string(), list()) -> [{string(), string()}].
create_request(OAuthData, URL, Params) -> 
    SignedRequest = sign_request(OAuthData, URL, Params),
    {AuthorizationParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, SignedRequest),
    lager:debug("A:~p~n, Q:~p~n", [AuthorizationParams, QueryParams]),
    {oauth:uri(URL, QueryParams), [oauth:header(AuthorizationParams)]}.

