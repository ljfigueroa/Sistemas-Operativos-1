-module(pbalance).
-compile(export_all).

get_server(Pbalance) ->
    io:fwrite("Modulo pbalance y el metodo get_server(~p) ~n", [Pbalance]),
    Pbalance ! {req, self()},
    receive
        {ok, Server} -> {ok, Server}
    end.

