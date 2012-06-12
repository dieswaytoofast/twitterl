%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @end
%%%-------------------------------------------------------------------
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
%%
%% Errors
%%
-define(AUTH_ERROR, authentication_error).

%% Twitter
-define(TWITTERL_RECEIVER, twitterl_receiver).
%% Gproc
-define(GPROC_REGISTRATION_ERROR, gproc_registration_error).
-define(GPROC_UNKNOWN_PROCESS, gproc_unknown_process).
%% Timer
-define(DEFAULT_TIMER_TIMEOUT, 5000).


%%
%% Types
%%
-type error()                                 :: {error, Reason :: term()}.
-type time()                                  :: non_neg_integer().
-type timer2_ref()                            :: {reference(), reference()}.
-type timer2_server_ref()                     :: {pid(), reference()}.
-type child_type()                            :: twitter_receiver.

