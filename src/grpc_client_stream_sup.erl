-module(grpc_client_stream_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(ParamsMap) ->
    supervisor:start_link(?MODULE, ParamsMap).

init(#{pool_id := PoolId} = ParamsMap) ->    
    PoolSpecs = {PoolId, {poolboy, start_link,
        [[{name, {local, PoolId}},
        {worker_module, grpc_client_stream},
        {size, 10},
        {max_overflow, 200}],
    ParamsMap]},
    permanent, 5000, worker,
    [poolboy]},

    Children = [PoolSpecs],

    {ok, {{one_for_all, 10, 10}, Children}}.

