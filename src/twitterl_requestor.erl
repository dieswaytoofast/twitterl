%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Module serving twitterl_requestor functions
%%% @end
%%%-------------------------------------------------------------------
-module(twitterl_requestor).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([get_request/1, get_request/2, process_request/3, process_request/4,
         stop_request/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").
-include("twitterl_requestor.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc get the http Request associated with the URL
-spec get_request(URL::string()) -> [{string(), string()}].
get_request(URL) ->
    get_request(URL, []).

%% @doc get the http Request associated with the URL and Params
-spec get_request(URL::string(), Params::list()) -> [{string(), string()}].
get_request(URL, Params) ->
    OAuthData = get_oauth_data(),
    create_request(OAuthData, URL, Params).

%% @doc Run the request on the URL in stream or REST mode. The result will be
%%          sent to Target
-spec process_request(Target::target(), URL::string(), RequestType::rest|stream) -> any().
process_request(Target, RequestType, URL) -> 
    process_request(Target, RequestType, URL, []).

%% @doc Run the request on the URL and Params in stream or REST mode. The result will be
%%          sent to Target
-spec process_request(Target::target(), RequestType::rest|stream, URL::string(), Params::list()) -> {ok, pid()} | error().
process_request(Target, RequestType, URL, Params) -> 
    twitterl_util:validate_request_type(RequestType),
    Request = get_request(URL, Params),
    twitterl_manager:safe_call({?TWITTERL_PROCESSOR, RequestType}, {request, Request, RequestType, Target}).

%% @doc Stop a given request gracefully
-spec stop_request(RequestId::request_id()) -> ok.
stop_request({ServerProcess, RequestPid}) ->
    gen_server:cast(ServerProcess, {stop_request, RequestPid}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    twitterl_manager:register_process(?TWITTERL_REQUESTOR, ?TWITTERL_REQUESTOR),
    State = #requestor_state{},
    {ok, State}.

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
-spec get_oauth_data() -> #twitter_oauth_data{}.
get_oauth_data() ->
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

