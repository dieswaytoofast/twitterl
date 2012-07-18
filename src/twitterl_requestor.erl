%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Twitterl module that accepts requests
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(twitterl_requestor).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').


-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([process_request/7, process_request/8, stop_request/1]).

% Authorization
-export([get_consumer/0, get_request_token/1, get_access_token/3]).
% Status
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%%% Request processing
%% @doc Run the request on the URL and Params in stream or REST mode. The result will be
%%          sent to Target
-spec process_request(target(), request_type(), http_request_type(), url(), params(), token(), secret()) -> {ok, request_reference()} | error().
process_request(Target, RequestType, HttpRequestType, URL, Params, Token, Secret) -> 
    process_request(Target, RequestType, HttpRequestType, URL, Params, Token, Secret, ?TWITTERL_ITEM_TYPE_TWEET).

-spec process_request(target(), request_type(), http_request_type(), url(), params(), token(), secret(), item_type()) -> {ok, request_reference()} | error().
process_request(Target, RequestType, HttpRequestType, URL, Params, Token, Secret, ItemType) -> 
    Consumer = get_consumer(),
    SToken = util:get_string(Token),
    SSecret = util:get_string(Secret),
    SendFun = get_send_fun(ItemType),
    twitterl_manager:safe_call({?TWITTERL_PROCESSOR, RequestType}, {request, Target, RequestType, HttpRequestType, URL, Params, Consumer, SToken, SSecret, SendFun}).

-spec get_send_fun(item_type()) -> function().
get_send_fun(ItemType) ->
    fun(Dest, Data) -> lager:debug("Data:~p~n", [Data]), send_to_target(ItemType, Dest, Data) end.

%% @doc Stop a given request gracefully
-spec stop_request(RequestId::request_id()) -> ok.
stop_request({ServerProcess, RequestPid}) ->
    gen_server:cast(ServerProcess, {stop_request, RequestPid}).

%%% Authorization
%% @doc Get the consumer for this app
-spec get_consumer() -> consumer().
get_consumer() ->
    twitterl_manager:safe_call(?TWITTERL_REQUESTOR, {get_consumer}).

%% @doc Get a request token
-spec get_request_token(url()) -> #twitter_token_data{} | error().
get_request_token(TargetURL) ->
    twitterl_manager:safe_call(?TWITTERL_REQUESTOR, {get_request_token, TargetURL}).

%% @doc Get a request token
-spec get_access_token(verifier(), token(), secret()) -> #twitter_access_data{} | error().
get_access_token(Verifier, Token, Secret) ->
    twitterl_manager:safe_call(?TWITTERL_REQUESTOR, {get_access_token, Verifier, Token, Secret}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    twitterl_manager:register_process(?TWITTERL_REQUESTOR, ?TWITTERL_REQUESTOR),
    OAuthData = get_oauth_data(),
    State = #requestor_state{oauth_data = OAuthData},
    {ok, State}.

handle_call({get_request_token, TargetURL}, From, State) ->
    {RequestType, HttpRequestType, URL} = ?TWITTER_REQUEST_TOKEN,
    Target  = {gen_server, From},

    STargetURL = util:get_string(TargetURL),
    Params = [{"oauth_callback", STargetURL}],

    OAuthData = State#requestor_state.oauth_data,
    Consumer = get_consumer(OAuthData),

    SToken = "",
    SSecret = "",

    % Send reply to the invoker
    SendFun = fun(Dest, Data) -> send_token_to_target(Dest, Data) end,
    spawn_request(RequestType, Target, HttpRequestType, URL, Params, Consumer, SToken, SSecret, SendFun),
    {noreply, State};

handle_call({get_access_token, Verifier, Token, Secret}, From, State) ->
    {RequestType, HttpRequestType, URL} = ?TWITTER_ACCESS_TOKEN,
    Target  = {gen_server, From},

    SVerifier = util:get_string(Verifier),
    Params = [{"oauth_verifier", SVerifier}],

    OAuthData = State#requestor_state.oauth_data,
    Consumer = get_consumer(OAuthData),

    SToken = util:get_string(Token),
    SSecret = util:get_string(Secret),

    % Send reply to the invoker
    SendFun = fun(Dest, Data) -> send_access_to_target(Dest, Data) end,
    spawn_request(RequestType, Target, HttpRequestType, URL, Params, Consumer, SToken, SSecret, SendFun),
    {noreply, State};

handle_call({get_consumer}, _From, State) ->
    OAuthData = State#requestor_state.oauth_data,
    Consumer = get_consumer(OAuthData),
    {reply, Consumer, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
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

-spec send_token_to_target(target(), list()) -> any().
send_token_to_target(Target, Tokens) ->
    try
        TokenData = validate_tokens(Tokens),
        twitterl_manager:respond_to_target(Target, TokenData)
    catch
        _:Error ->
            twitterl_manager:respond_to_target(Target, {error, Error})
    end.

-spec send_access_to_target(target(), list()) -> any().
send_access_to_target(Target, Data) ->
    try
        AccessData = extract_access(Data),
        twitterl_manager:respond_to_target(Target, AccessData)
    catch
        _:Error ->
            twitterl_manager:respond_to_target(Target, {error, Error})
    end.
    
-spec extract_access(list()) -> #twitter_access_data{}.
extract_access(Response) ->
    Data = oauth:uri_params_decode(Response),
    Token = get_token(Data),
    Secret = get_secret(Data),
    UserId = get_user_id(Data),
    ScreenName = get_screen_name(Data),
    #twitter_access_data{
        access_token = Token,
        access_token_secret = Secret,
        user_id = UserId,
        screen_name = ScreenName}.

-spec validate_tokens(list()) -> #twitter_token_data{} | error().
validate_tokens(Response) ->
    Tokens = oauth:uri_params_decode(Response),
    case proplists:get_value("oauth_callback_confirmed", Tokens) of
        "true" ->
            extract_tokens(Tokens);
        _ ->
            {error, ?INVALID_REQUEST_TYPE}
    end.


-spec extract_tokens(list()) -> #twitter_token_data{} | error().
extract_tokens(Tokens) ->
    Token = get_token(Tokens),
    Secret = get_secret(Tokens),
    #twitter_token_data{
        access_token = Token,
        access_token_secret = Secret}.

-spec get_token(list()) -> binary().
get_token(Tokens) ->
    case lists:keyfind("oauth_token", 1, Tokens) of
        {_, Token} -> util:get_binary(Token);
        false -> throw(?AUTH_ERROR)
    end.

-spec get_secret(list()) -> binary().
get_secret(Tokens) ->
    case lists:keyfind("oauth_token_secret", 1, Tokens) of
        {_, Token} -> util:get_binary(Token);
        false -> throw(?AUTH_ERROR)
    end.

-spec get_user_id(list()) -> binary().
get_user_id(Tokens) ->
    case lists:keyfind("user_id", 1, Tokens) of
        {_, Token} -> util:get_binary(Token);
        false -> throw(?AUTH_ERROR)
    end.

-spec get_screen_name(list()) -> binary().
get_screen_name(Tokens) ->
    case lists:keyfind("screen_name", 1, Tokens) of
        {_, Token} -> util:get_binary(Token);
        false -> throw(?AUTH_ERROR)
    end.


%% @doc Sends the data back to the calling process
%%      <<"\r\n">> needs to be ignored

send_to_target(_ItemType, _Target, <<"\r\n">>) ->
         ok;
send_to_target(ItemType, Target, BinBodyPart) ->
    try
        JsonBodyPart = ejson:decode(BinBodyPart),
        twitterl_parser:parse(ItemType, JsonBodyPart, Target)
    catch
        Class:Reason -> 
            lager:debug("catch send_tweet_to_target Class:~p~n, Reason:~p~n", [Class, Reason])
    end.

spawn_request(RequestType, Target, HttpRequestType, URL, Params, Consumer, Token, Secret, SendFun) ->
    proc_lib:spawn_link(
        fun() -> twitterl_manager:safe_call({?TWITTERL_PROCESSOR, RequestType}, {request, Target, RequestType, HttpRequestType, URL, Params, Consumer, Token, Secret, SendFun})
        end).

