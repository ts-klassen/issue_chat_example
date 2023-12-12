%%%-------------------------------------------------------------------
%% @doc issue_chat_example public API
%% @end
%%%-------------------------------------------------------------------

-module(issue_chat_example_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    issue_chat_example_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
