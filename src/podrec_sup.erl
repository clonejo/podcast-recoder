%%%-------------------------------------------------------------------
%% @doc podcast_recoder top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('podrec_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {#{strategy => one_for_one},
          [#{id => podrec_feeds, start => {podrec_feeds, start_link, []}}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
