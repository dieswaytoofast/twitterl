%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Module serving twitterl_parser functions
%%% @end
%%%-------------------------------------------------------------------
-module(twitterl_parser_util).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-compile([{parse_transform, lager_transform}]).

-include("defaults.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([parse/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

parse({?TWITTERL_ITEM_TYPE_TWEET, Values}) ->
	% Create a TweetRecord for this Tweet based on the values
	Tweet = format_tweet(Values),
    case is_tweet_empty(Tweet) of
        true ->
            {error, ?EMPTY_ERROR};
        _ ->
            {ok, Tweet}
    end;

parse({?TWITTERL_ITEM_TYPE_USER, Values}) ->
	% Create a TweetRecord for this Tweet based on the values
	Tweet = format_user(Values),
    case is_user_empty(Tweet) of
        true ->
            {error, ?EMPTY_ERROR};
        _ ->
            {ok, Tweet}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
is_tweet_empty(Tweet) ->
    if Tweet =:= #tweet{} ->
            true;
        true ->
            false
    end.

is_user_empty(User) ->
    if User =:= #twitter_user{} ->
            true;
        true ->
            false
    end.

%% ====================================================================
%% Tweet
%% ====================================================================
format_tweet(Values) ->
    PathList =
    lists:flatten([parse_tweet_field(Tuple) || Tuple <- Values]),
    record_util:update_record(#tweet{}, PathList).

parse_tweet_field({_Field = <<"user">>, {Value}}) -> 
    UserRecord = format_user(Value),
    {user, UserRecord};

parse_tweet_field({_Field = <<"entities">>, {Value}}) ->
    EntitiesRecord = format_entities(Value),
    {entities, EntitiesRecord};

parse_tweet_field({_Field = <<"place">>, null}) ->
    {place, undefined};

parse_tweet_field({_Field = <<"place">>, {Value}}) ->
    PlaceRecord = format_place(Value),
    {place, PlaceRecord};

parse_tweet_field({_Field = <<"geo">>, {Value}}) ->
    GeoRecord = format_bounding_box(Value),
    {geo, GeoRecord};


parse_tweet_field({_Field = <<"id">>, Value}) -> 
    {id, twitterl_util:get_integer(Value)};

parse_tweet_field({_Field = <<"id_str">>, Value}) ->
    {id_str, Value};

parse_tweet_field({_Field = <<"text">>, Value}) ->
    {text, Value};

parse_tweet_field({_Field = <<"coordinates">>, Value}) ->
    {coordinates, Value};


parse_tweet_field({_Field = <<"created_at">>, Value}) ->
    {created_at, Value};

parse_tweet_field({_Field, _Value}) -> [].

format_user(Values) ->
    PathList =
    lists:flatten([parse_user_field(Tuple) || Tuple <- Values]),
    record_util:update_record(#twitter_user{}, PathList).

parse_user_field({_Field = <<"id">>, Value}) -> 
    {id, twitterl_util:get_integer(Value)};

parse_user_field({_Field = <<"id_str">>, Value}) ->
    {id_str, Value};


parse_user_field({_Field = <<"name">>, Value}) ->
    {name, Value};

parse_user_field({_Field = <<"screen_name">>, Value}) ->
    {screen_name, Value};

parse_user_field({_Field = <<"location">>, Value}) ->
    {location, Value};

parse_user_field({_Field = <<"description">>, Value}) ->
    {description, Value};

parse_user_field({<<"profile_image_url">>, Value}) ->
    {profile_image_url, Value};

parse_user_field({<<"utc_offset">>, Value}) ->
    {utc_offset, twitterl_util:get_integer(Value)};

parse_user_field({<<"time_zone">>, Value}) ->
    {time_zone, Value};

parse_user_field({<<"follwers_count">>, Value}) ->
    {follwers_count, twitterl_util:get_integer(Value)};

parse_user_field({<<"friends_count">>, Value}) ->
    {friends_count, twitterl_util:get_integer(Value)};

parse_user_field({<<"statuses_count">>, Value}) ->
    {statuses_count, Value};

parse_user_field({<<"lang">>, Value}) ->
    {lang, Value};

parse_user_field({<<"geo_enabled">>, Value}) ->
    {geo_enabled, Value};

parse_user_field({<<"status">>, {Values}}) ->
    Status = format_tweet(Values),
    {status, Status};

parse_user_field({_Field, _Value}) -> [].

format_place(Values) ->
    PathList =
    lists:flatten([parse_place_field(Tuple) || Tuple <- Values]),
    record_util:update_record(#twitter_place{}, PathList).

parse_place_field({_Field = <<"id">>, Value}) -> 
    {id, Value};

parse_place_field({_Field = <<"url">>, Value}) ->
    {url, Value};

parse_place_field({_Field = <<"place_type">>, Value}) ->
    {place_type, Value};

parse_place_field({_Field = <<"name">>, Value}) ->
    {name, Value};

parse_place_field({_Field = <<"full_name">>, Value}) ->
    {full_name, Value};

parse_place_field({_Field = <<"country_code">>, Value}) ->
    {country_code, Value};

parse_place_field({_Field = <<"country">>, Value}) ->
    {country, Value};

parse_place_field({_Field = <<"bounding_box">>, {Value}}) ->
    BoundingBoxRecord = format_bounding_box(Value),
    {bounding_box, BoundingBoxRecord};


parse_place_field({_Field, _Value}) -> [].

format_bounding_box(Values) ->
    PathList =
    lists:flatten([parse_bounding_box_field(Tuple) || Tuple <- Values]),
    record_util:update_record(#bounding_box{}, PathList).

parse_bounding_box_field({_Field = <<"type">>, Value}) ->
    {type, Value};

parse_bounding_box_field({_Field = <<"coordinates">>, [Value]}) ->
    {coordinates, Value};

parse_bounding_box_field({_Field, _Value}) -> [].

format_entities(Values) ->
    PathList =
    lists:flatten([parse_entities(Tuple) || Tuple <- Values]),
    record_util:update_record(#entities{}, PathList).

parse_entities({_Field = <<"hashtags">>, Value}) ->
    Tags = lists:foldl(fun({X}, Acc) ->
                    Tag = twitterl_util:keysearch(<<"text">>, 1, <<>>, X),
                    [Tag|Acc]
            end, [], Value),
    {hashtags, Tags};

parse_entities({_Field = <<"urls">>, Value}) ->
    UrlsList = [format_urls(Item) || Item <- Value], 
    {urls, UrlsList};

parse_entities({_Field, _Value}) -> [].

format_urls({Values}) ->
    PathList =
    lists:flatten([parse_urls(Tuple) || Tuple <- Values]),
    record_util:update_record(#entity_url{}, PathList).

parse_urls({_Field = <<"expanded_url">>, Value}) ->
    {expanded_url, Value};

parse_urls({_Field, _Value}) -> [].
