-module(client).
-import(server,[init/1]).
-compile(export_all).

start() ->
    Port = 8000 + random:uniform(100),
    server:init([Port]),
    %%timer:sleep(3000),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
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
    exit(normal).

send(Sock, Msg) ->
    gen_tcp:send(Sock, Msg),
    receive
	       {tcp, Sock, Data} -> io:fwrite("OK - client send <~p> and receive <~p>~n", [Msg, Data])
    after
	       1000 -> io:fwrite("ERROR - client send <~p> and didn't receive a response~n", [Msg])
    end,
    ok.
