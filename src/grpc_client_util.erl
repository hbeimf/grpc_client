
-module(grpc_client_util).

-export([monitor_connection/1]).

monitor_connection(#{http_connection := Pid}) ->
    erlang:monitor(process, Pid),
    ok;
monitor_connection(_Connection) -> 
    ok.

