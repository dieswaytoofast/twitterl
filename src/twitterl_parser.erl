%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Module serving twitterl_parser functions
%%% @end
%%%-------------------------------------------------------------------
-module(twitterl_parser).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([parse/2, parse/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

-record(parser_state, {
            }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
parse(ItemType, JsonBody) ->
    twitterl_manager:safe_call(?TWITTERL_PARSER, {parse, ItemType, JsonBody, {debug, undefined}}).

parse(ItemType, JsonBody, Target) ->
    twitterl_manager:safe_cast(?TWITTERL_PARSER, {parse, ItemType, JsonBody, Target}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    process_flag(trap_exit, true),
    twitterl_manager:register_process(?TWITTERL_PARSER, make_ref()),
    State = #parser_state{},
    {ok, State}.

handle_call({parse, ItemType, JsonBody, Target}, _From, State) ->
    Reply = parse_internal(ItemType, JsonBody, Target),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({parse, ItemType, JsonBody, Target} = _Req, State) ->
    _Pid = proc_lib:spawn_link(fun() -> parse_internal(ItemType, JsonBody, Target) end),
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
parse_internal(ItemType, JsonBody, Target) when is_list(JsonBody) ->
    lists:map(fun
            ({Item}) ->
                case twitterl_parser_util:parse({ItemType, Item}) of
                    {ok, Record} ->
                        twitterl_manager:respond_to_target(Target, Record);
                    _ ->
                        void
                end
        end,  JsonBody);

%%  Deal with JsonBody of {[{<<"friends">>,[]}]}
parse_internal(ItemType, {JsonBody}, Target) ->
    case twitterl_parser_util:parse({ItemType, JsonBody}) of
        {ok, Record} ->
            twitterl_manager:respond_to_target(Target, Record);
        {error, ?EMPTY_ERROR} ->
            void;
        Error ->
            twitterl_manager:respond_to_target(Target, Error)
    end.
