%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012  Mahesh Paolini-Subramanya
%%% @doc twitter_receiver header files and definitions.
%%% @end
%%%-------------------------------------------------------------------
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

%%
%% Errors
%%


%% Timeouts
-define(CONNECTION_RETRY_DURATION, 10000).
-define(CONNECTION_TIMEOUT_DURATION, 60000).
%% Records
-record(processor_state, {
            requests = dict:new()
            }).

-record(request_details, {
            pid :: pid(),
            target                  :: target(),
            request_type            :: request_type(),
            http_request_type       :: http_request_type(),
            url                     :: url(),
            params                  :: params(),
            consumer                :: consumer(),
            token                   :: token(),
            secret                  :: secret(),
            send_function           :: function(),
            retry_count             :: integer()
            }).
