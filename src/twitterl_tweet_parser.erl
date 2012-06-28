%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Module serving twitterl_tweet_parser functions
%%% @end
%%%-------------------------------------------------------------------
-module(twitterl_tweet_parser).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([parse_many_tweets/2]).
-export([parse_one_tweet/1, parse_one_tweet/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").
-include("twitterl_tweet_parser.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
parse_many_tweets(JsonBody, Target) ->
    lager:debug("JsonBody:~p~n, Target:~p~n", [JsonBody, Target]),
    twitterl_manager:safe_cast(?TWITTERL_TWEET_PARSER, {parse_many_tweets, JsonBody, Target}).

parse_one_tweet(JsonBody) ->
    lager:debug("JsonBody:~p~n", [JsonBody]),
    twitterl_manager:safe_call(?TWITTERL_TWEET_PARSER, {parse_one_tweet, JsonBody}).

parse_one_tweet(JsonBody, Target) ->
    lager:debug("JsonBody:~p~n, Target:~p~n", [JsonBody, Target]),
    twitterl_manager:safe_cast(?TWITTERL_TWEET_PARSER, {parse_one_tweet, JsonBody, Target}).



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    process_flag(trap_exit, true),
    twitterl_manager:register_process(?TWITTERL_TWEET_PARSER, make_ref()),
    State = #tweet_parser_state{},
    {ok, State}.

handle_call({parse_one_tweet, JsonBody}, _From, State) ->
    lager:debug("1.5, ~p~n", [JsonBody]),
    Reply = parse_one_tweet_internal(JsonBody),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    lager:debug("2, ~p~n", [_Request]),
    {reply, ok, State}.

handle_cast({parse_many_tweets, JsonBody, Target} = _Req, State) ->
    lager:debug("3, ~p~n", [_Req]),
    _Pid = proc_lib:spawn_link(fun() -> parse_many_tweets_internal(JsonBody, Target) end),
    {noreply, State};

handle_cast({parse_one_tweet, JsonBody, Target} = _Req, State) ->
    lager:debug("3, ~p~n", [_Req]),
    _Pid = proc_lib:spawn_link(fun() -> parse_one_tweet_internal(JsonBody, Target) end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT',  _Pid, {ok, _} = _Reason}, State) ->
    {noreply, State};

%% Json parsing barfed somewhere
handle_info({'EXIT',  _Pid, {error, _} = Reason}, State) ->
    lager:debug("Error Reason:~p~n", [Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    lager:debug("Info, ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:debug("terminate, ~p~n", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
parse_many_tweets_internal(JsonBody, Target) ->
    lists:map(fun
            ({Tweet}) ->
                case twitterl_parser_util:parse({tweet, Tweet}) of
                    {ok, TweetRecord} ->
                        twitterl_manager:respond_to_target(Target, TweetRecord);
                    _ ->
                        void
                end
        end,  JsonBody).

%%  Deal with JsonBody of {[{<<"friends">>,[]}]}
parse_one_tweet_internal({JsonBody}) ->
    case twitterl_parser_util:parse({tweet, JsonBody}) of
        {ok, TweetRecord} ->
            TweetRecord;
        Error ->
            Error
    end.

%%  Deal with JsonBody of {[{<<"friends">>,[]}]}
parse_one_tweet_internal({JsonBody}, Target) ->
    case twitterl_parser_util:parse({tweet, JsonBody}) of
        {ok, TweetRecord} ->
            twitterl_manager:respond_to_target(Target, TweetRecord);
        Error ->
            twitterl_manager:respond_to_target(Target, Error)
    end.


