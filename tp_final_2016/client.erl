-module(client).
-import(server,[init/1]).
-compile(export_all).

start() ->
    Port = 8005,
    server:init([Port]),
    timer:sleep(1000),
    io:format("conecting to psocket\n"),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock,"CON lauro").


