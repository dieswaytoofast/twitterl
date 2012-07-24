%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Main module for the twitterl application.
%%% @end
%%%-------------------------------------------------------------------
-module(twitterl_tests).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("../src/defaults.hrl").
-include_lib("eunit/include/eunit.hrl").


%% ------------------------------------------------------------------
%% Defines
%% ------------------------------------------------------------------

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(foreach(F), {foreach, fun start/0, fun stop/1, F}).

%%% Dummy data 

%% ------------------------------------------------------------------
%% Test Function Definitions
%% ------------------------------------------------------------------

%%
%% Test Descriptions
%%
twitterl_test_() ->
%    [{"twitterl tests",
%      ?setup(fun() -> [fun t_account_verify_credentials_test/1, fun t_statuses_update_test/1] end)}].
    {setup,
     fun start/0,
     fun stop/1,
     fun (_) ->
                [t_account_verify_credentials_test(), 
                 t_account_clear_home_timeline_test(),
                 t_statuses_update_test()] end}.

%%
%% Setup Functions
%%
start() ->
    twitterl:start().


stop(_) ->
    twitterl:stop().



%%
%% Helper Functions
%%

t_account_verify_credentials_test() ->
    Token = twitterl:get_env(oauth_access_token),
    Secret = twitterl:get_env(oauth_access_token_secret),
    Name = twitterl:get_env(oauth_consumer_name),
    Result = twitterl:account_verify_credentials({self, self}, [], Token, Secret),
    ?_assertEqual(bstr:bstr(Name), Result#twitter_user.screen_name).


t_account_clear_home_timeline_test() ->
    Token = twitterl:get_env(oauth_access_token),
    Secret = twitterl:get_env(oauth_access_token_secret),
    clean_home_timeline(Token, Secret),
    Result = 
    try 
        twitterl:statuses_home_timeline({self, self}, [], Token, Secret)
    catch
        _:{Type, _} ->
            Type
    end,
    ?_assertEqual(Result, timeout).

t_statuses_update_test() ->
    Token = twitterl:get_env(oauth_access_token),
    Secret = twitterl:get_env(oauth_access_token_secret),
    Status = "Test One",
    Result = twitterl:statuses_update({self, self}, [{"status", Status}], Token, Secret),
    ?_assertEqual(bstr:bstr(Status), Result#tweet.text).

clean_home_timeline(Token, Secret) ->
    Pid = self(),
    proc_lib:spawn_link(fun() -> 
                timer:sleep(1000), 
                twitterl:statuses_home_timeline({process, Pid}, [], Token, Secret)
        end),
    remove_entries(Token, Secret).


remove_entries(Token, Secret) ->
    receive
        {'EXIT', _Pid, Reason} ->
            Reason;
        Message ->
            SId = util:get_string(Message#tweet.id_str),
            twitterl:statuses_destroy({self, self}, SId, [], Token, Secret),
            remove_entries(Token, Secret)
    after 3000 ->
            timeout
    end.




