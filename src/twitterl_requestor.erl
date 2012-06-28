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
-export([get_request/2, get_request/3, get_request/5, process_request/3, process_request/4,
         stop_request/1]).

% Authorization
-export([get_request_token/0, get_access_token/3]).
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


%%% Viewing requests

%% @doc get the http Request associated with the URL
-spec get_request(method, url()) -> [{string(), string()}].
get_request(Method, URL) ->
    get_request(Method, URL, []).

%% @doc get the http Request associated with the URL and Params
-spec get_request(method(), url(), params()) -> [{string(), string()}].
get_request(Method, URL, Params) ->
    OAuthData = get_oauth_data(),
    create_request(OAuthData, Method, URL, Params).

%% @doc get the http Request associated with the URL and Params
-spec get_request(method(), url(), params(), token(), secret()) -> [{string(), string()}].
get_request(Method, URL, Params, Token, Secret) ->
    OAuthData = get_oauth_data(),
    create_request(OAuthData, Method, URL, Params, Token, Secret).


%%% Request processing

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

%%% Authorization

%% @doc Get a request token
get_request_token() ->
    Consumer = get_consumer(get_oauth_data()),
    Response = check_response(oauth:post(?TWITTER_REQUEST_TOKEN_URL, [{"oauth_callback", "http://www.posttestserver.com"}], Consumer)), 
    Tokens = oauth:params_decode(Response),
    validate_tokens(Tokens).

%% @doc Get a request token
get_access_token(Token, Secret, Verifier) ->
    Consumer = get_consumer(get_oauth_data()),
    Response = check_response(oauth:post(?TWITTER_ACCESS_TOKEN_URL, [{"oauth_verifier", Verifier}], Consumer, Token, Secret)),
    oauth:params_decode(Response).

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
-spec get_consumer(#twitter_oauth_data{}) -> consumer().
get_consumer(OAuthData) ->
    {OAuthData#twitter_oauth_data.consumer_key,
     OAuthData#twitter_oauth_data.consumer_secret,
     hmac_sha1}.

%% @doc Sign the Request
-spec sign_request(#twitter_oauth_data{}, string_method(), url(), params()) -> [{string(), string()}].
sign_request(OAuthData, StringMethod, URL, Params) -> 
    Token = OAuthData#twitter_oauth_data.access_token, 
    Secret = OAuthData#twitter_oauth_data.access_token_secret, 
    sign_request(OAuthData, StringMethod, URL, Params, Token, Secret).

-spec sign_request(#twitter_oauth_data{}, string_method(), url(), params(), token(), secret()) -> [{string(), string()}].
sign_request(OAuthData, StringMethod, URL, Params, Token, Secret) -> 
    Consumer = get_consumer(OAuthData),
    oauth:sign(StringMethod, URL, Params, Consumer, Token, Secret).

%% @doc Create the Request
-spec create_request(#twitter_oauth_data{}, method(), url(), params()) -> [{string(), string()}].
create_request(OAuthData, Method, URL, Params) -> 
    StringMethod = twitterl_util:get_string_method(Method),
    SignedRequest = sign_request(OAuthData, StringMethod, URL, Params),
    build_request(URL, SignedRequest).

-spec create_request(#twitter_oauth_data{}, method(), url(), params(), token(), secret()) -> [{string(), string()}].
create_request(OAuthData, Method, URL, Params, Token, Secret) -> 
    StringMethod = twitterl_util:get_string_method(Method),
    SignedRequest = sign_request(OAuthData, StringMethod, URL, Params, Token, Secret),
    build_request(URL, SignedRequest).

build_request(URL, SignedRequest) ->
    {AuthorizationParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, SignedRequest),
    lager:debug("A:~p~n, Q:~p~n", [AuthorizationParams, QueryParams]),
    {oauth:uri(URL, QueryParams), [oauth:header(AuthorizationParams)]}.

validate_tokens(Tokens) ->
    case proplists:get_value("oauth_callback_confirmed", Tokens) of
        "true" ->
            Tokens;
        _ ->
            throw({error, ?INVALID_REQUEST_TYPE})
    end.

%% @doc Check the http request for errors
-spec check_response(any()) -> any().
check_response(Response) ->
    case Response of
        {ok, {{_, 401, _} = _Status, _Headers, _Body} = _Response} ->
            lager:debug("Response:~p~n", [Response]),
            throw({error, ?AUTH_ERROR});
        {ok, Response} ->
            Response;
        Other ->
            throw({error, Other})
    end.

