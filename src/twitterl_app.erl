-module(twitterl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    twitterl_requestor_sup:start_link(),
    twitterl_processor_sup:start_link(),
    twitterl_parser_sup:start_link().

stop(_State) ->
    ok.
