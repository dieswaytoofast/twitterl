%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc oauth authentication fsm
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------

%% TYPES
%% Tuple containing a date and time.
-type datetime()                                :: {calendar:date(), {calendar:hour(), calendar:minute(), calendar:second() | float()}}.
%% A floating point number representing the number of seconds elapsed since
%% Jan 1, 1970, 00:00:00 (Unix epoch).
-type epoch()                                   :: non_neg_integer() | float().


-record(twitter_oauth_data, {
            consumer_key        :: string(),
            consumer_secret     :: string(),
            access_token         :: string(),
            access_token_secret  :: string()
            }).

-record(twitter_token_data, {
            access_token            :: binary(),
            access_token_secret     :: binary()
            }).

-record(twitter_access_data, {
            access_token            :: binary(),
            access_token_secret     :: binary(),
            user_id                 :: binary(),
            screen_name             :: binary()
            }).

-record(requestor_state, {
            oauth_data          :: #twitter_oauth_data{}
            }).

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
            created_at    :: epoch(),
            profile_image_url   :: any(),
            utc_offset    :: integer(),
            time_zone     :: binary(),
            followers_count :: integer(),
            friends_count :: integer(),
            statuses_count :: integer(),
            lang          :: binary(),
            geo_enabled   :: boolean(),
            status        :: any()  %% Actually %tweet, but can'd do it
            }).

-record(entity_url, {
            url           :: binary(),
            expanded_url  :: binary(),
            display_url   :: binary()
            }).

-record(entities, {
            hashtags = [] :: list(),
            urls = []     :: list()
            }).
            
-record(tweet, {
            id_str        :: binary(),
            id            :: integer(),
            in_reply_to_status_id_str      :: binary(),
            in_reply_to_status_id          :: integer(),
            in_reply_to_user_id_str        :: binary(),
            in_reply_to_user_id            :: integer(),
            in_reply_to_screen_name        :: binary(),
            text          :: binary(),
            coordinates   :: any(),
            place         :: any(),
            geo           :: #bounding_box{},
            created_at    :: epoch(),
            user          :: #twitter_user{},
            entities      :: #entities{}
            }).




