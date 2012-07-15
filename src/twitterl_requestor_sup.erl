%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Main module for the twitterl_requestor supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(twitterl_requestor_sup).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_requestor/0]).

%% Supervisor callbacks
-export([init/1]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Type, Module, Args), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_requestor() ->
    supervisor:start_child(?MODULE, ?CHILD(make_ref(), worker, ?TWITTERL_REQUESTOR, [])).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    NumRequestors = twitterl:get_env(?TWITTERL_REQUESTOR_COUNT, 1),
    Requestors = lists:foldl(fun(_X, Acc) -> 
                    [?CHILD(make_ref(), worker, ?TWITTERL_REQUESTOR, []) | Acc]
            end, [], lists:seq(1, NumRequestors)),
    {ok, { {one_for_one, 5, 300}, Requestors} }.
