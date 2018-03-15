-module(client).
-import(server,[init/1]).
-compile(export_all).

start() ->
    Port = 8000 + random:uniform(100),
    server:init([Port]),
    io:format("5 - conecting to psocket\n"),
						%timer:sleep(3000),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    io:format("Sending data... CON lauro\n"),
    ok = gen_tcp:send(Sock,"    CON     lauro       "),
    ok = gen_tcp:close(Sock),    
    timer:sleep(3000),
    exit(kill).
