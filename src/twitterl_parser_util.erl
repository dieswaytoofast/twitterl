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
-export([twitter_time_to_epoch/1, twitter_time_to_datetime/1]).

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
    {id, Value};

parse_tweet_field({_Field = <<"id_str">>, Value}) ->
    {id_str, Value};


parse_tweet_field({_Field = <<"in_reply_to_user_id">>, Value}) -> 
    {in_reply_to_user_id, Value};

parse_tweet_field({_Field = <<"in_reply_to_user_id_str">>, Value}) ->
    {in_reply_to_user_id_str, Value};


parse_tweet_field({_Field = <<"in_reply_to_status_id">>, Value}) -> 
    {in_reply_to_status_id, Value};

parse_tweet_field({_Field = <<"in_reply_to_status_id_str">>, Value}) ->
    {in_reply_to_status_id_str, Value};


parse_tweet_field({_Field = <<"in_reply_to_screen_name">>, Value}) -> 
    {in_reply_to_screen_name, Value};

parse_tweet_field({_Field = <<"text">>, Value}) ->
    {text, Value};

parse_tweet_field({_Field = <<"coordinates">>, Value}) ->
    {coordinates, Value};


parse_tweet_field({_Field = <<"created_at">>, Value}) ->
    {created_at, twitter_time_to_epoch(Value)};

parse_tweet_field({_Field, _Value}) -> [].

format_user(Values) ->
    PathList =
    lists:flatten([parse_user_field(Tuple) || Tuple <- Values]),
    record_util:update_record(#twitter_user{}, PathList).

parse_user_field({_Field = <<"id">>, Value}) -> 
    {id, Value};

parse_user_field({_Field = <<"id_str">>, Value}) ->
    {id_str, Value};


parse_user_field({_Field = <<"name">>, Value}) ->
    {name, Value};

parse_user_field({_Field = <<"screen_name">>, Value}) ->
    {screen_name, Value};

parse_user_field({_Field = <<"created_at">>, Value}) ->
    {created_at, twitter_time_to_epoch(Value)};


parse_user_field({_Field = <<"location">>, Value}) ->
    {location, Value};

parse_user_field({_Field = <<"description">>, Value}) ->
    {description, Value};

parse_user_field({<<"profile_image_url">>, Value}) ->
    {profile_image_url, Value};

parse_user_field({<<"utc_offset">>, Value}) ->
    {utc_offset, Value};

parse_user_field({<<"time_zone">>, Value}) ->
    {time_zone, Value};

parse_user_field({<<"follwers_count">>, Value}) ->
    {followers_count, Value};

parse_user_field({<<"friends_count">>, Value}) ->
    {friends_count, Value};

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

parse_bounding_box_field({_Field = <<"coordinates">>, Value}) ->
    {coordinates, Value};

parse_bounding_box_field({_Field, _Value}) -> [].

format_entities(Values) ->
    PathList =
    lists:flatten([parse_entities(Tuple) || Tuple <- Values]),
    record_util:update_record(#entities{}, PathList).

parse_entities({_Field = <<"hashtags">>, Value}) ->
    Tags = lists:foldl(fun({X}, Acc) ->
                    Tag = case lists:keyfind(<<"text">>, 1, X) of
                        {_, Val} -> Val;
                        false -> <<>>
                    end,
                    [Tag|Acc]
            end, [], Value),
    {hashtags, Tags};

parse_entities({_Field = <<"urls">>, Value}) ->
    UrlsList = [build_strings(Item) || Item <- Value], 
    {urls, UrlsList};

parse_entities({_Field, _Value}) -> [].

build_strings({Values}) ->
    PathList =
    lists:flatten([parse_urls(Tuple) || Tuple <- Values]),
    record_util:update_record(#entity_url{}, PathList).

parse_urls({_Field = <<"expanded_url">>, Value}) ->
    {expanded_url, Value};

parse_urls({_Field, _Value}) -> [].

%% ====================================================================
%% Time manipulation
%% ====================================================================
%% @doc Convert a date and time as binary string in the ISO 8601 format to the
%%      number of seconds since the Unix epoch (Jan 1, 1970, 00:00:00) with
%%      millisecond precision.
-spec twitter_time_to_epoch(binary()) -> epoch().
twitter_time_to_epoch(TwitterDatetime) when is_binary(TwitterDatetime) ->
    util:datetime_to_epoch(twitter_time_to_datetime(TwitterDatetime));
twitter_time_to_epoch(_TwitterDatetime) ->
    null.

%% @doc Convert a datetime in the format used by Twitter to a date and time in the
%%      format returned by calendar:universal_time/0.
-spec twitter_time_to_datetime(binary()) -> datetime().
twitter_time_to_datetime(<<_DDD:3/binary, " ", MMM:3/binary, " ", DD:2/binary, " ",
                      Hh:2/binary, $:, Mm:2/binary, $:, Ss:2/binary, " ",
                      Sign, TzHour:2/binary, TzMin:2/binary, " ", YYYY:4/binary, _Tail/binary>>) ->
    Month = month(MMM),
    Date1 = {bstr:to_integer(YYYY), Month, bstr:to_integer(DD)},
    Hour1 = bstr:to_integer(Hh),
    Min1 = bstr:to_integer(Mm),
    Sec1 = bstr:to_integer(Ss),

    if
        TzHour =:= <<"00">> andalso TzMin =:= <<"00">> ->
            {Date1, {Hour1, Min1, Sec1}};
        true ->
            LocalSec = calendar:datetime_to_gregorian_seconds({Date1, {Hour1, Min1, Sec1}}),
            %% Convert the the seconds in the local timezone to UTC.
            UtcSec = case ((bstr:to_integer(TzHour) * 3600 + bstr:to_integer(TzMin)) * 60) of
                         Offset when Sign =:= $- -> LocalSec - Offset;
                         Offset                  -> LocalSec + Offset
                     end,
            calendar:gregorian_seconds_to_datetime(UtcSec)
    end;
twitter_time_to_datetime(_TwitterDatetime) ->
    null.


-spec month(binary()) -> integer().
month(<<"Jan">>) ->  1;
month(<<"Feb">>) ->  2;
month(<<"Mar">>) ->  3;
month(<<"Apr">>) ->  4;
month(<<"May">>) ->  5;
month(<<"Jun">>) ->  6;
month(<<"Jul">>) ->  7;
month(<<"Aug">>) ->  8;
month(<<"Sep">>) ->  9;
month(<<"Oct">>) -> 10;
month(<<"Nov">>) -> 11;
month(<<"Dec">>) -> 12.


