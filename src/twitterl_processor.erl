%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Module serving twitterl_processor functions
%%% @end
%%%-------------------------------------------------------------------
-module(twitterl_processor).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").
-include("twitterl_processor.hrl").

-record(state, {
            pid :: pid()
            }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link(FromPid, Request) ->
    gen_server:start_link(?MODULE, [FromPid, Request], []).


init([FromPid, Request]) ->
    process_flag(trap_exit, true),
    lager:debug("1,FromPid:~p, Request:~p~n", [FromPid, Request]),
    Pid = proc_lib:spawn_link(fun() -> return_data(FromPid, Request) end),
    State = #state{pid = Pid},
    {ok, State}.


handle_call(_Request, _From, State) ->
    lager:debug("3, ~p~n", [_Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT',  Pid, {error, _} = Reason}, State) when State#state.pid =:= Pid ->
    lager:debug("Stopped on error, ~p~n", [Reason]),
    {stop, Reason, State};

handle_info({'EXIT',  Pid, {ok, Reason}}, State) when State#state.pid =:= Pid ->
    lager:debug("Stopped on ok, ~p~n", [Reason]),
    {stop, normal, State};

handle_info(_Info, State) ->
    lager:debug("Info, ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:debug("terminate, ~p~n", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
return_data(FromPid, Request) ->
    try 
        httpc:request(get, Request, [], [{stream, self}, {sync, false}]) of
        {ok, RequestId} ->
            case receive_data(FromPid, RequestId) of
                {ok, _} ->
                    %% Connection closed normally.  Redo it
                    timer:sleep(?CONNECTION_RETRY_DURATION),
                    return_data(FromPid, Request);
                %% Exit 'normal', so that the supervisor doesnt restart it
                {error, ?AUTH_ERROR, Reason} ->
                    lager:debug("error authenticating Reason:~p~n", [Reason]),
                    erlang:exit({ok, Reason});
                {error, Reason} ->
                    lager:debug("error Reason:~p~n", [Reason]),
                    erlang:exit({error, Reason});
                Reason  ->
                    lager:debug("Unknown Reason:~p~n", [Reason]),
                    erlang:exit({error, Reason})
            end;
        Response ->
            lager:debug("Unknown Response:~p~n", [Response]),
            erlang:exit({error, Response})
    catch 
        Class:Reason -> 
            lager:debug("catch return_data Class:~p~n, Reason:~p~n", [Class, Reason]),
            erlang:exit({error, {Class, Reason}})
    end.
 
%%====================================================================
%% Internal functions
%%====================================================================
receive_data(FromPid, RequestId) ->
    receive
        {http, {RequestId, {error, Reason}}} 
                when (Reason =:= timeout) 
                orelse (Reason =:= etimedout) -> 
            lager:debug("1, error, Reason:~p~n", [Reason]),
            {error, timeout};
        {http, {RequestId, {{_, 401, _} = _Status, _Headers, _}} = Result} -> 
            lager:debug("2, error, Reason:~p~n", [Result]),
            {error, ?AUTH_ERROR, Result};
        {http, {RequestId, Result}} -> 
            lager:debug("3, error, Reason:~p~n", [Result]),
            {error, Result};
        %% Data starts
        {http,{RequestId, stream_start, _Headers}} ->
            lager:debug("4, stream_start:~n", []),
            receive_data(FromPid, RequestId);
        %% Data continues
        {http,{RequestId, stream, BinBodyPart}} ->
            lager:debug("5, stream, BinBodyPart:~p~n", [BinBodyPart]),
            proc_lib:spawn_link(fun() -> send_data(FromPid, BinBodyPart) end),
            receive_data(FromPid, RequestId);
        %% Data ends
        {http,{RequestId, stream_end, _Headers}} ->
            lager:debug("6, stream_end~n", []),
            {ok, RequestId}
    after ?CONNECTION_TIMEOUT_DURATION ->
            lager:debug("7, timeout~n", []),
            receive_data(FromPid, RequestId)
    end.

%% @doc Sends the data back to the calling process
send_data(ToPid, BinBodyPart) ->
    try
        JsonBodyPart = ejson:decode(BinBodyPart),
        lager:debug("returning data to:~p, JsonBodyPart:~p~n, BinBodyPart:~p~n", [ToPid, JsonBodyPart, BinBodyPart]),
        ToPid ! {data, JsonBodyPart}
    catch
        Class:Reason -> 
            lager:debug("catch send_data Class:~p~n, Reason:~p~n", [Class, Reason])
    end.
