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

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").
-include("twitterl_processor.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link(RequestType) ->
    gen_server:start_link(?MODULE, [RequestType], []).


init([RequestType]) ->
    process_flag(trap_exit, true),
    twitterl_manager:register_process(?TWITTERL_PROCESSOR, RequestType),
    State =  #processor_state{},
    {ok, State}.


handle_call({request, Target, RequestType, HttpRequestType, URL, Params, Consumer, Token, Secret, SendFun} = _Req, _From, State) ->
    lager:debug("request, ~p~n", [_Req]),
    RetryCount = application:get_env(retry_count, ?TWITTERL_RETRY_COUNT),
    Pid = proc_lib:spawn_link(fun() -> return_data(Target, RequestType, HttpRequestType, URL, Params, Consumer, Token, Secret, SendFun) end),
    RequestDetails = #request_details{pid = Pid, 
                                      target = Target, 
                                      request_type = RequestType,
                                      http_request_type = HttpRequestType,
                                      url = URL,
                                      params = Params,
                                      consumer = Consumer,
                                      token = Token,
                                      secret = Secret,
                                      send_function = SendFun,
                                      retry_count = RetryCount},
    OldDict = State#processor_state.requests,
    NewDict = dict:store(Pid, RequestDetails, OldDict),
    NewState = State#processor_state{requests = NewDict},
    {reply, {ok, {self(), Pid}}, NewState};

handle_call(_Request, _From, State) ->
    lager:debug("3, ~p~n", [_Request]),
    {reply, ok, State}.

handle_cast({stop_request, Pid}, State) ->
    OldDict = State#processor_state.requests,
    case dict:is_key(Pid, OldDict) of
        true ->
            erlang:exit(Pid, {ok, stop_request});
        _E ->
            void
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT',  Pid, Reason}, State) ->
    lager:debug("Exited on Reason, ~p~n", [Reason]),
    OldDict = State#processor_state.requests,
    NewDict = 
    case dict:find(Pid, OldDict) of
        {ok, OldRequest} ->
            Dict1 = dict:erase(Pid, OldDict),
            case Reason of
                normal ->
                    Dict1;
                {ok, _} ->
                    Dict1;
                {error, ?AUTH_ERROR} ->
                    Dict1;
                {error, _} ->
                    NewRetryCount = OldRequest#request_details.retry_count - 1,
                    if NewRetryCount =< 0 ->
                            lager:debug("No more retries for Request:~p~n", [OldRequest]),
                            Dict1;
                        true -> 
                            Target = OldRequest#request_details.target,
                            RequestType = OldRequest#request_details.request_type,
                            HttpRequestType = OldRequest#request_details.http_request_type,
                            URL = OldRequest#request_details.url,
                            Params = OldRequest#request_details.params,
                            Consumer = OldRequest#request_details.consumer,
                            Token = OldRequest#request_details.token,
                            Secret = OldRequest#request_details.secret,
                            SendFun = OldRequest#request_details.send_function,
                            NewPid = proc_lib:spawn_link(fun() -> return_data(Target, RequestType, HttpRequestType, URL, Params, Consumer, Token, Secret, SendFun) end),
                            NewRequestDetails = OldRequest#request_details{
                                    pid = NewPid,
                                    retry_count = NewRetryCount},
                            dict:store(pid, NewRequestDetails, Dict1)
                    end
            end;
        _ ->
            OldDict
    end,
    {noreply, State#processor_state{requests = NewDict}};

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
return_data(Target, _RequestType = rest, HttpRequestType, URL, Params, Consumer, Token, Secret, SendFun) ->
    try
        OauthFun = get_oauth_fun(HttpRequestType, URL, Params, Consumer, Token, Secret),
        case OauthFun() of
            {ok, {{_, 401, _} = _Status, _Headers, _Body} = _Response} ->
                lager:debug("Response:~p~n", [_Response]),
                twitterl_manager:respond_to_target(Target, {error, ?AUTH_ERROR});
            {ok, {{_, 403, _} = _Status, _Headers, Body} = _Response} ->
                lager:debug("Response:~p~n", [_Response]),
                twitterl_manager:respond_to_target(Target, {error, Body});
            {ok, {_Result, _Headers, BinBody}} ->
                try 
                    SendFun(Target, BinBody)
                catch
                    IClass:IReason ->
                        lager:debug("catch send_data IClass:~p~n, IReason:~p~n", [IClass, IReason]),
                        twitterl_manager:respond_to_target(Target, {error, IReason})
                end;
            Response ->
                lager:debug("Unknown Response:~p~n", [Response])
        end
    catch 
        OClass:OReason -> 
            lager:debug("catch return_data OClass:~p~n, OReason:~p~n", [OClass, OReason])
    end;

return_data(Target, RequestType = stream, HttpRequestType, URL, Params, Consumer, Token, Secret, SendFun) ->
    try
        OauthFun = get_oauth_fun(HttpRequestType, URL, Params, Consumer, Token, Secret, [{stream, self}, {sync, false}]),
        case OauthFun() of
            {ok, RequestId} ->
                case receive_data(Target, RequestId, SendFun) of
	                %% Connection closed normally.  Redo it
                    {ok, _} ->
	                    timer:sleep(?CONNECTION_RETRY_DURATION),
                        return_data(Target, RequestType, HttpRequestType, URL, Params, Consumer, Token, Secret, SendFun);
	                %% Exit 'normal', so that the supervisor doesnt restart it
	                {error, ?AUTH_ERROR, Reason} ->
	                    lager:debug("error authenticating Reason:~p~n", [Reason]),
                        twitterl_manager:respond_to_target(Target, {error, Reason}),
                        {error, ?AUTH_ERROR};
	                {error, ?RATE_ERROR, Reason} ->
	                    lager:debug("error with too many connections Reason:~p~n", [Reason]),
	                    timer:sleep(?CONNECTION_RETRY_DURATION),
                        return_data(Target, RequestType, HttpRequestType, URL, Params, Consumer, Token, Secret, SendFun);
	                {error, Reason} ->
	                    lager:debug("error Reason:~p~n", [Reason]),
                        twitterl_manager:respond_to_target(Target, {error, Reason}),
                        {error, Reason};
	                Reason  ->
	                    lager:debug("Unknown Reason:~p~n", [Reason]),
                        twitterl_manager:respond_to_target(Target, {error, Reason}),
                        {error, Reason}
	            end;
	        Response ->
	            lager:debug("Unknown Response:~p~n", [Response]),
                {error, Response}
            end
    catch 
        OClass:OReason -> 
            lager:debug("catch return_data OClass:~p~n, OReason:~p~n", [OClass, OReason]),
            {error, OReason}
    end.
 
%%====================================================================
%% Internal functions
%%====================================================================
receive_data(Target, RequestId, SendFun) ->
    receive
        {http, {RequestId, {error, Reason}}} 
                when (Reason =:= timeout) 
                orelse (Reason =:= etimedout) -> 
            lager:debug("1, error, Reason:~p~n", [Reason]),
            {error, timeout};
        {http, {RequestId, {{_, 401, _} = _Status, _Headers, _}} = Result} -> 
            lager:debug("2, error, Reason:~p~n", [Result]),
            {error, ?AUTH_ERROR, Result};
        {http, {RequestId, {{_, 420, _} = _Status, _Headers, _}} = Result} -> 
            lager:debug("2, error, Reason:~p~n", [Result]),
            {error, ?RATE_ERROR, Result};
        {http, {RequestId, Result}} -> 
            lager:debug("3, error, Reason:~p~n", [Result]),
            {error, Result};
        %% Data starts
        {http,{RequestId, stream_start, _Headers}} ->
            lager:debug("4, stream_start:~n", []),
            receive_data(Target, RequestId, SendFun);
        %% Data continues
        {http,{RequestId, stream, BinBodyPart}} ->
            lager:debug("5, stream, BinBodyPart:~p~n", [BinBodyPart]),
            proc_lib:spawn_link(fun() -> SendFun(Target, BinBodyPart) end),
            receive_data(Target, RequestId, SendFun);
        %% Data ends
        {http,{RequestId, stream_end, _Headers}} ->
            lager:debug("6, stream_end~n", []),
            {ok, RequestId}
    after ?CONNECTION_TIMEOUT_DURATION ->
            lager:debug("7, timeout~n", []),
            receive_data(Target, RequestId, SendFun)
    end.


get_oauth_fun(get, URL, Params, Consumer, Token, Secret) ->
    fun() -> oauth:get(URL, Params, Consumer, Token, Secret) end;
get_oauth_fun(post, URL, Params, Consumer, Token, Secret) ->
    fun() -> oauth:post(URL, Params, Consumer, Token, Secret) end.
get_oauth_fun(get, URL, Params, Consumer, Token, Secret, HttpArgs) ->
    fun() -> oauth:get(URL, Params, Consumer, Token, Secret, HttpArgs) end;
get_oauth_fun(post, URL, Params, Consumer, Token, Secret, HttpArgs) ->
    fun() -> oauth:post(URL, Params, Consumer, Token, Secret, HttpArgs) end.
