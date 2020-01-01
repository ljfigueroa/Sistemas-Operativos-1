-module(pstat).
-compile(export_all).

pstat() ->
    receive
    after
        3000 ->
            {ok, Servers} = server:get_servers(),
            {Total_Reductions, _} = erlang:statictics(reductions),
            Fun = (fun(S) -> {server:notify(S, Total_Reductions)} end),
            list:map(Fun, Servers),
            pstat()
    end.
