-module(client).
-import(server,[init/1]).
-compile(export_all).

start() ->
    Port = 8000 + random:uniform(100),
    server:init([Port]),
    %%timer:sleep(3000),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    ok = send(Sock, "    CON     Lauro       "),
    ok = send(Sock, io_lib:format(" LSG  ~p ", [make_ref()])),
    ok = send(Sock, io_lib:format(" NEW  ~p ", [make_ref()])),
    ok = send(Sock, io_lib:format(" ACC  ~p  ~p ", [make_ref(), "GameId"])),
    ok = send(Sock, io_lib:format(" PLA  ~p  ~p  ~p ", [make_ref(), "GameId", "Move"])),
    ok = send(Sock, io_lib:format(" OBS  ~p  ~p ", [make_ref(), "GameId"])),
    ok = send(Sock, io_lib:format(" LEA  ~p  ~p ", [make_ref(), "GameId"])),
    ok = send(Sock, " BYE  "),
    ok = gen_tcp:close(Sock),
    %%timer:sleep(5000),
    exit(normal).

send(Sock, Msg) ->
    gen_tcp:send(Sock, Msg),
    receive
	{tcp, Sock, Data} -> io:format("OK - client send <~s> and receive <~p>~n", [Msg, Data])
    after
	1000 -> io:fwrite("ERROR - client send <~p> and didn't receive a response~n", [Msg])
    end,
    ok.
