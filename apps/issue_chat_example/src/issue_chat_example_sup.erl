%%%-------------------------------------------------------------------
%% @doc issue_chat_example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(issue_chat_example_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 3,
                 period => 1},
    ChildSpecs = [
        #{
            id => issue_chat_example_on_issue_open_webhook_worker
          , start => {issue_chat_example, on_issue_open_webhook_worker, []}
          , restart => permanent
          , type => worker
        }
      , #{
            id => issue_chat_example_on_comment_create_webhook_worker
          , start => {issue_chat_example, on_comment_create_webhook_worker, []}
          , restart => permanent
          , type => worker
        }
      , #{
            id => issue_chat_example_run_worker
          , start => {issue_chat_example, run_worker, []}
          , restart => permanent
          , type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
