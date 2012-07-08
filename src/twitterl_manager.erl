%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Mahesh Paolini-Subramanya
%%% @doc Module providing twitterl management helper function
%%% @end
%%%-------------------------------------------------------------------
-module(twitterl_manager).

-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_process/1]).
-export([register_process/2]).
-export([safe_cast/2, safe_call/2, safe_call/3]).
-export([respond_to_target/2]).


%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("defaults.hrl").

%% ------------------------------------------------------------------
%% Defines
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Register a process locally
-spec register_process(Type::atom(),Name:: atom()|binary() | reference()) -> true.
register_process(Type, Name) ->
    gproc:reg({p, l, Type}, Name).

%% @doc Get the Pid for a given process type, or {Type, Name}
-spec get_process({Type::atom(), Name::atom()} | atom()) -> pid() | error().
get_process(Key) ->
    get_child_pid(Key).

%% @doc Unified mechanism to send a gen_server call request to a supervised process
-spec safe_call({Type :: atom(), Name :: atom()} | atom(), Request :: any()) -> {ok, pid()} | {ok, Result :: any(), pid()} | error().
safe_call({_Type, _Name} = Key, Request) ->
    safe_call(Key, Request, ?DEFAULT_TIMER_TIMEOUT);

safe_call(Type, Request) ->
    safe_call(Type, Request, ?DEFAULT_TIMER_TIMEOUT).


-spec safe_call({Type :: atom(), Name :: atom()} | atom(), Request::any(), timeout()) -> {ok, pid()} | {ok, Result :: any(), pid()} | error().
safe_call({Type, Name}, Request, Timeout) ->
    % Send the request to the process
    case get_process({Type, Name}) of 
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Request, Timeout);
        _ ->
            {ok, Target} = start_supervised_process(Type, Name),
            gen_server:call(Target, Request, Timeout)
    end;

safe_call(Type, Request, Timeout) ->
    % Send the request to the process
    case get_process(Type) of 
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Request, Timeout);
        _ ->
            {ok, Target} = start_supervised_process(Type, Type),
            gen_server:call(Target, Request, Timeout)
    end.

%% @doc Unified mechanism to send a gen_server cast request to a supervised process
-spec safe_cast({Type :: atom(), Name :: atom()} | atom(), Request :: any()) -> ok | error().
safe_cast({Type, Name}, Request) ->
    % Send the request to the process
    case get_process({Type, Name}) of 
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Request);
        _ ->
            {ok, Target} = start_supervised_process(Type, Name),
            gen_server:cast(Target, Request)
    end;

safe_cast(Type, Request) ->
    % Send the request to the process
    case get_process(Type) of 
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Request);
        _ ->
            {ok, Target} = start_supervised_process(Type, Type),
            gen_server:cast(Target, Request)
    end.

-spec respond_to_target(Target::target(), Message::any()) -> ok.
respond_to_target(Target, Message) ->
    try
        respond_internal(Target, Message)
    catch
        _:Error ->
            lager:debug("Error:~p sending to Target:~p, Message:~p~n~n", [Error, Target, Message])
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_supervised_process(?TWITTERL_PROCESSOR, ?TWITTERL_REQUEST_TYPE_STREAM) ->
    twitterl_processor_sup:start_processor(?TWITTERL_REQUEST_TYPE_STREAM);
start_supervised_process(?TWITTERL_PROCESSOR, ?TWITTERL_REQUEST_TYPE_REST) ->
    twitterl_processor_sup:start_processor(?TWITTERL_REQUEST_TYPE_REST).

get_child_pid({Type, Name}) ->
    % If the process was started, it will be registered in gproc
    case gproc:select({local, all}, [{{{p, l, Type}, '$1', Name}, [], ['$1']}]) of
        [Pid] when is_pid(Pid) ->
            Pid;
        [] ->
            {error, ?GPROC_UNKNOWN_PROCESS}
    end;

get_child_pid(Type) ->
    % For one Pid, just return it
    case gproc:lookup_pids({p, l, Type}) of
        [Pid] ->
            Pid;
        PidList when is_list(PidList) ->
            random:seed(os:timestamp()),
            Index = random:uniform(length(PidList)),
            lists:nth(Index, PidList)
    end.

-spec respond_internal(target(), Message::any()) -> ok.
respond_internal({debug, _}, Message) ->
    lager:debug("Message:~p~n", [Message]);
respond_internal({process, Target}, Message) ->
    Target ! Message;
respond_internal({gen_server, Target}, Message) ->
    gen_server:reply(Target, Message);
respond_internal({self, _}, Message) ->
    Message.
