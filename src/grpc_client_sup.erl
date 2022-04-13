%%%-------------------------------------------------------------------
%%% Licensed to the Apache Software Foundation (ASF) under one
%%% or more contributor license agreements.  See the NOTICE file
%%% distributed with this work for additional information
%%% regarding copyright ownership.  The ASF licenses this file
%%% to you under the Apache License, Version 2.0 (the
%%% "License"); you may not use this file except in compliance
%%% with the License.  You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%

-module(grpc_client_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([pool_id/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    PoolId = pool_id(),
    ParamsMap = #{'Service' => undefined, 'Rpc' => undefined, 'Encoder' => undefined, 'Options' => undefined},
    
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

pool_id() ->
    stream_pool_id.
