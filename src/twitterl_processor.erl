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
                {error, ?DEAD_PROCESS} ->
                    Dict1;
                {error, _} ->
                    NewRetryCount = OldRequest#request_details.retry_count - 1,
                    if NewRetryCount =< 0 ->
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
    {noreply, State}.

terminate(_Reason, _State) ->
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
                twitterl_manager:respond_to_target(Target, {error, ?AUTH_ERROR});
            {ok, {{_, 403, _} = _Status, _Headers, Body} = _Response} ->
                twitterl_manager:respond_to_target(Target, {error, Body});
            {ok, {_Result, _Headers, BinBody}} ->
                try 
                    SendFun(Target, BinBody)
                catch
                    IClass:IReason ->
                        lager:debug("catch return_data IClass:~p~n, IReason:~p~n", [IClass, IReason]),
                        twitterl_manager:respond_to_target(Target, {error, IReason})
                end;
            Response ->
                twitterl_manager:respond_to_target(Target, {error, Response})
        end
    catch 
        OClass:OReason -> 
            lager:debug("catch return_data OClass:~p~n, OReason:~p~n", [OClass, OReason]),
            twitterl_manager:respond_to_target(Target, {error, OReason})
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
                        twitterl_manager:respond_to_target(Target, {error, Reason}),
                        {error, ?AUTH_ERROR};
	                {error, ?RATE_ERROR, _Reason} ->
	                    timer:sleep(?CONNECTION_RETRY_DURATION),
                        return_data(Target, RequestType, HttpRequestType, URL, Params, Consumer, Token, Secret, SendFun);
	                {error, Reason} ->
                        twitterl_manager:respond_to_target(Target, {error, Reason}),
                        {error, Reason};
	                Reason  ->
                        twitterl_manager:respond_to_target(Target, {error, Reason}),
                        {error, Reason}
	            end;
	        Response ->
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
            {error, timeout};
        {http, {RequestId, {{_, 401, _} = _Status, _Headers, _}} = Result} -> 
            {error, ?AUTH_ERROR, Result};
        {http, {RequestId, {{_, 420, _} = _Status, _Headers, _}} = Result} -> 
            {error, ?RATE_ERROR, Result};
        {http, {RequestId, Result}} -> 
            {error, Result};
        %% Data starts
        {http,{RequestId, stream_start, _Headers}} ->
            receive_data(Target, RequestId, SendFun);
        %% Data continues
        {http,{RequestId, stream, BinBodyPart}} ->
            validate_target(Target),
            proc_lib:spawn_link(fun() -> SendFun(Target, BinBodyPart) end),
            receive_data(Target, RequestId, SendFun);
        %% Data ends
        {http,{RequestId, stream_end, _Headers}} ->
            {ok, RequestId}
    after ?CONNECTION_TIMEOUT_DURATION ->
            receive_data(Target, RequestId, SendFun)
    end.


% Need to validate that the target actually exists for functions too
% TODO multiple nodes?
validate_target({process, Pid}) ->
    case is_process_alive(Pid) of
        true ->
            true;
        false ->
            erlang:exit(self, {error, ?DEAD_PROCESS})
    end;
validate_target(_Target) ->
    ok.


get_oauth_fun(get, URL, Params, Consumer, Token, Secret) ->
    fun() -> oauth:get(URL, Params, Consumer, Token, Secret) end;
get_oauth_fun(post, URL, Params, Consumer, Token, Secret) ->
    fun() -> oauth:post(URL, Params, Consumer, Token, Secret) end.
get_oauth_fun(get, URL, Params, Consumer, Token, Secret, HttpArgs) ->
    fun() -> oauth:get(URL, Params, Consumer, Token, Secret, HttpArgs) end;
get_oauth_fun(post, URL, Params, Consumer, Token, Secret, HttpArgs) ->
    fun() -> oauth:post(URL, Params, Consumer, Token, Secret, HttpArgs) end.
