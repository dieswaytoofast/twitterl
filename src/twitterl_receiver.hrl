%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012  Mahesh Paolini-Subramanya
%%% @doc twitterl_receiver header files and definitions.
%%% @end
%%%-------------------------------------------------------------------
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

%%
%% Errors
%%

%% Twitter API URLs
-define(TWITTER_FAVORITES_URL, "https://api.twitter.com/1/favorites.json").
-define(TWITTER_HOME_TIMELINE_URL, "https://api.twitter.com/1/statuses/home_timeline.json").


%% Records
-record(receiver_state, {
            max_id = 0              :: integer(),
            since_id = 0            :: integer(),
            oauth_data          :: twitter_oauth_data
            }).

-record(twitter_oauth_data, {
            consumer_key        :: string(),
            consumer_secret     :: string(),
            access_token         :: string(),
            access_token_secret  :: string()
            }).
