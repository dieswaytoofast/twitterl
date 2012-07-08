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
-export([parse_tweets/1, parse_tweets/2]).

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
parse_tweets(JsonBody) ->
    twitterl_manager:safe_call(?TWITTERL_TWEET_PARSER, {parse_tweets, JsonBody}).

parse_tweets(JsonBody, Target) ->
    twitterl_manager:safe_cast(?TWITTERL_TWEET_PARSER, {parse_tweets, JsonBody, Target}).


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

handle_call({parse_tweets, JsonBody}, _From, State) ->
    Reply = parse_tweets_internal(JsonBody),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({parse_tweets, JsonBody, Target} = _Req, State) ->
    _Pid = proc_lib:spawn_link(fun() -> parse_tweets_internal(JsonBody, Target) end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT',  _Pid, {ok, _} = _Reason}, State) ->
    {noreply, State};

%% Json parsing barfed somewhere
handle_info({'EXIT',  _Pid, {error, _} = _Reason}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
parse_tweets_internal(JsonBody, Target) when is_list(JsonBody) ->
    lists:map(fun
            ({Tweet}) ->
                case twitterl_parser_util:parse({tweet, Tweet}) of
                    {ok, TweetRecord} ->
                        twitterl_manager:respond_to_target(Target, TweetRecord);
                    _ ->
                        void
                end
        end,  JsonBody);

%%  Deal with JsonBody of {[{<<"friends">>,[]}]}
parse_tweets_internal({JsonBody}, Target) ->
    case twitterl_parser_util:parse({tweet, JsonBody}) of
        {ok, TweetRecord} ->
            twitterl_manager:respond_to_target(Target, TweetRecord);
        {error, ?EMPTY_ERROR} ->
            void;
        Error ->
            twitterl_manager:respond_to_target(Target, Error)
    end.

%%  Deal with JsonBody of {[{<<"friends">>,[]}]}
parse_tweets_internal({JsonBody}) ->
    case twitterl_parser_util:parse({tweet, JsonBody}) of
        {ok, TweetRecord} ->
            TweetRecord;
        Error ->
            Error
    end.


