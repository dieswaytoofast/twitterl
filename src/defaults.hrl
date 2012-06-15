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
-define(RATE_ERROR, rate_error).
-define(INVALID_REQUEST_TYPE, invalid_request_type).
-define(EMPTY_ERROR, empty).
-define(INVALID_BOOLEAN, invalid_boolean).

%% Twitter
-define(TWITTERL_PROCESSOR_SUP, twitterl_processor_sup).
-define(TWITTERL_PROCESSOR, twitterl_processor).
-define(TWITTERL_TWEET_PARSER_SUP, twitterl_tweet_parser_sup).
-define(TWITTERL_TWEET_PARSER, twitterl_tweet_parser).
-define(TWITTERL_REQUESTOR_SUP, twitterl_requestor_sup).
-define(TWITTERL_REQUESTOR, twitterl_requestor).
-define(TWITTERL_REST_PROCESSOR, twitterl_rest_processor).
-define(TWITTERL_STREAM_PROCESSOR, twitterl_stream_processor).
-define(TWITTERL_REQUEST_TYPE_REST, rest).
-define(TWITTERL_REQUEST_TYPE_STREAM, stream).
-define(TWITTERL_TWEET_PARSER_COUNT, 2).
-define(TWITTERL_REQUESTOR_COUNT, 2).
-define(TWITTERL_RETRY_COUNT, 5).

%% Gproc
-define(GPROC_REGISTRATION_ERROR, gproc_registration_error).
-define(GPROC_UNKNOWN_PROCESS, gproc_unknown_process).

%% Timer
-define(DEFAULT_TIMER_TIMEOUT, 5000).
%% Processor
-define(REQUEST_COMPLETED, request_completed).


%%
%% Types
%%
-type error()                                 :: {error, Reason :: term()}.
-type request_type()                          :: stream|rest.
-type process_name()                          :: request_type().
-type target()                                :: pid() | atom() | fun().
-type request_id()                                :: {pid(), pid()}.

-record(bounding_box, {
            type          :: binary(),
            coordinates   :: list()
            }).

-record(twitter_place, {
            id            :: binary(),
            url           :: binary(),
            place_type    :: binary(),
            name          :: binary(),
            full_name     :: binary(),
            country_code  :: binary(),
            country       :: binary(),
            bounding_box  :: #bounding_box{}
            }).

-record(twitter_user, {
            id_str        :: binary(),
            id            :: integer(),
            name          :: binary(),
            screen_name   :: binary(),
            location      :: any(),
            description   :: any(),
            profile_image_url   :: any()
            }).

-record(entity_url, {
            url           :: binary(),
            expanded_url  :: binary(),
            display_url   :: binary()
            }).

-record(entities, {
            hashtags      :: list(),
            urls          :: list()
            }).
            
-record(tweet, {
            id_str        :: binary(),
            id            :: integer(),
            text          :: binary(),
            coordinates   :: any(),
            place         :: any(),
            created_at    :: any(),
            user          :: #twitter_user{},
            entities      :: #entities{}
            }).

