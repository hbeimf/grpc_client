-module(grpc_client_stream_sup_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_stream_pool/1, stop_stream_pool/1]).
-export([children/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

% -include_lib("glib/include/log.hrl").
%%====================================================================
%% API functions
%%====================================================================
children() ->
  supervisor:which_children(?SERVER).

% grpc_client_stream_sup_sup:start_stream_pool(ParamsMap).
% ParamsMap = #{'Transport' => tcp, 'Host' => "localhost", 'Port' => 10000, pool_id => PoolId},
start_stream_pool(#{'Transport' := Transport, 'Host' := Host, 'Port' := Port, pool_id := PoolId} = ParamsMap) ->
    case whereis(PoolId) of 
      undefined ->
        case grpc_client:connect(Transport, Host, Port) of 
          {ok, Connection} ->
            grpc_client:stop_connection(Connection),
            supervisor:start_child(?SERVER, [ParamsMap]),
            {ok, PoolId};
          _ ->
            false
        end;
      _Any ->
        {ok, PoolId}
    end.

% cli:stop_stream_pool().
stop_stream_pool(#{pool_id := PoolId}) ->
    Children = children(),
    lists:foreach(fun({_, ChildPid, _, _}) -> 
      ChildList = supervisor:which_children(ChildPid),
      case has_child(ChildList, PoolId) of 
        true ->
          supervisor:terminate_child(?SERVER, ChildPid),
          ok;
        _ ->
          ok
      end,
      ok    
    end, Children),
    ok.

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

has_child([], _Id) -> 
    false;
has_child([{Id, _, _, _}|_ChildList], Id) -> 
    true;
has_child([{_, _, _, _}|ChildList], Id) -> 
    has_child(ChildList, Id).


