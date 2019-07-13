-module(client).
-import(server,[init/1]).
-compile(export_all).

start() ->
    Port = 8000 + random:uniform(100),
    server:init([Port]),
    %%timer:sleep(3000),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    io:format("Sending data...     CON     Lauro       \n"),
    ok = send(Sock,"    CON     Lauro       "),
    ok = send(Sock,"LSG"),
    ok = send(Sock,"NEW"),
    ok = send(Sock,"ACC"),
    ok = send(Sock,"PLA"),
    ok = send(Sock,"OBS"),
    ok = send(Sock,"LEA"),
    ok = send(Sock,"BYE"),
    ok = gen_tcp:close(Sock),
    %%timer:sleep(5000),
    exit(kill).

send(Sock, Msg) ->
    gen_tcp:send(Sock, Msg),
    receive
	{tcp, Sock, Data} -> io:fwrite("CLIENT:RECEIVE <~p>~n", [Data])
    after
	1000 -> io:fwrite("Client didn't receive a response of the message: <~p>~n", [Msg])
    end,
    ok.
