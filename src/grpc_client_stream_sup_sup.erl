-module(grpc_client_stream_sup_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_stream_pool/1]).
-export([children/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
children() ->
  supervisor:which_children(?SERVER).

start_stream_pool(#{pool_id := PoolId} = ParamsMap) ->
    case whereis(PoolId) of 
      undefined ->
        supervisor:start_child(?SERVER, [ParamsMap]);
      Any ->
        Any
    end.


start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 6,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  ChildSup = child_sup(grpc_client_stream_sup),

  {ok, {SupFlags, [ChildSup]}}.

%%====================================================================
%% Internal functions
%%====================================================================
child_sup(Mod) ->
  Child = {Mod, {Mod, start_link, []},
    temporary, 5000, supervisor, [Mod]},
  Child.
