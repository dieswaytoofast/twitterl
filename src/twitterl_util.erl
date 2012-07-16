%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Module serving twitterl_util functions
%%% @end
%%%-------------------------------------------------------------------
-module(twitterl_util).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-include("defaults.hrl").

-export([validate_request_type/1]).
-export([validate_http_request_type/1]).
-export([keysearch/3, keysearch/4]).
-export([required/2]).
-export([validate_list_of_binaries/2]).

-export([get_boolean/1]).
-export([get_string/1]).
-export([get_binary/1]).
-export([get_integer/1]).

-export([build_string/1]).

%%
%% General Utilities
%% 
%% @doc: Search the key in the list of tuples and returns the value if it exists or throws an exception if it doesn't.
-spec keysearch(Key::term(), N::integer(), TupleList::[tuple()]) -> term().
keysearch(Key, _N, []) ->
    throw({undefined, Key});
keysearch(Key, N, TupleList) ->
    case lists:keyfind( Key, N, TupleList ) of
        {Key, Value} ->
            Value;
        false ->
            throw({undefined, Key})
    end.

%% @doc: Search the key in the list of tuples and returns the value if it exists or the default value if it doesn't.
-spec keysearch(Key::term(), N::integer(), Default::term(), TupleList::[tuple()]) -> term().
keysearch(_Key, _N, Default, []) ->
    Default;
keysearch(Key, N, Default, TupleList) ->
    case lists:keyfind( Key, N, TupleList ) of
        {Key, Value} ->
            Value;
        false ->
            Default
    end.

-spec get_string(atom() | binary() | string()) -> string().
get_string(Data) when is_atom(Data) -> atom_to_list(Data);
get_string(Data) when is_binary(Data) -> binary_to_list(Data);
get_string(Data) when is_list(Data) -> Data;
get_string(_) -> {error, ?INVALID_STRING}.

-spec get_binary(atom() | binary() | string()) -> binary().
get_binary(Data) when is_atom(Data) -> list_to_binary(atom_to_list(Data));
get_binary(Data) when is_list(Data) -> list_to_binary(Data);
get_binary(Data) when is_binary(Data) -> Data;
get_binary(_) -> {error, ?INVALID_BINARY}.


%%
%% Parameters
%% 
build_string(Params) when is_list(Params) ->
    Result = 
    lists:foldr(fun(X, Acc) ->
                    SX = get_string(X),
                    [SX|Acc]
            end, [], Params),
    lists:flatten(Result).



%%
%% Validations
%% 

%% @doc Check if Value is an 'empty' parameter
-spec validate_request_type(Type::request_type()) -> ok.
validate_request_type(?TWITTERL_REQUEST_TYPE_STREAM) ->
    ok;
validate_request_type(?TWITTERL_REQUEST_TYPE_REST) ->
    ok;
validate_request_type(_) ->
    throw({error, ?INVALID_REQUEST_TYPE}).

-spec validate_http_request_type(Type::http_request_type()) -> ok.
validate_http_request_type(?TWITTERL_HTTP_REQUEST_TYPE_GET) ->
    ok;
validate_http_request_type(?TWITTERL_HTTP_REQUEST_TYPE_POST) ->
    ok;
validate_http_request_type(_) ->
    throw({error, ?INVALID_REQUEST_TYPE}).

%% @doc Check if Value is an 'empty' parameter
-spec required(Field::term(), Value::term()) -> ok | error().
required(Field, Value) ->
    if Value =:= []
         orelse Value =:= <<>>
         orelse Value =:= undefined ->
           {error, ?EMPTY_ERROR, [Field]};
        true ->
            ok
    end.

%% @doc Validate that this is a list of binaries
-spec validate_list_of_binaries(Value::any(), ReturnVal::any()) -> ok | error().
validate_list_of_binaries([H|T], ReturnVal) ->
    if is_binary(H) =:= true ->
            validate_list_of_binaries(T, ReturnVal);
        true ->
            {error, ReturnVal, [H]}
    end;
validate_list_of_binaries([], _ReturnVal) ->
    ok.

%%
%% Parameter Extraction
%% 


%% @doc Gets the boolean value of the provided parameter
-spec get_boolean(Value::term()) -> boolean() | error().
get_boolean(Value) when is_binary(Value) -> 
    get_boolean_lower_value(bstr:lower(Value));
get_boolean(Value) -> 
    {error, ?INVALID_BOOLEAN, [Value]}.

get_boolean_lower_value(<<"true">>) -> true;
get_boolean_lower_value(<<"false">>) -> false;
get_boolean_lower_value(Value) -> 
    {error, ?INVALID_BOOLEAN, [Value]}.

-spec get_integer(term()) -> integer() | error().
get_integer(Value) ->
    case bstr:is_numeric(bstr:bstr(Value)) of
        true ->
            get_integer_value(Value);
        false ->
            {error, {?INVALID_INTEGER, [Value]}}
    end.

-spec get_integer_value(term()) -> integer().
get_integer_value(Value) when is_binary(Value) ->
    get_integer_value(binary_to_list(Value));
get_integer_value(Value) when is_atom(Value) ->
    get_integer_value(atom_to_list(Value));
get_integer_value(Value) when is_list(Value) ->
    list_to_integer(Value);
get_integer_value(Value) when is_integer(Value) ->
    Value.

