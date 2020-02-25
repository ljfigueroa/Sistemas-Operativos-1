-module(client).
-import(server,[init/1]).
-compile(export_all).

start() ->
    Port = 8000 + random:uniform(100),
    start(Port).

start(Port)->
    server:init([Port]),
    %%timer:sleep(3000),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    ok = send(Sock, "    CON     Lauro       "),
    ok = send(Sock, io_lib:format(" LSG  ~p ", [getUniqueId()])),
    ok = send(Sock, io_lib:format(" NEW  ~p ", [getUniqueId()])),
    ok = send(Sock, io_lib:format(" LSG  ~p ", [getUniqueId()])),
    ok = send(Sock, io_lib:format(" ACC  ~p  ~p ", [getUniqueId(), getUniqueId()])),
    ok = send(Sock, io_lib:format(" LSG  ~p ", [getUniqueId()])),
    ok = send(Sock, io_lib:format(" PLA  ~p  ~p  ~p ", [make_ref(), "GameId", "Move"])),
    ok = send(Sock, io_lib:format(" OBS  ~p  ~p ", [getUniqueId(), getUniqueId()])),
    ok = send(Sock, io_lib:format(" LEA  ~p  ~p ", [getUniqueId(), getUniqueId()])),
    ok = send(Sock, " BYE  "),
    ok = send(Sock, io_lib:format(" LSG  ~p ", [getUniqueId()])),
    ok = gen_tcp:close(Sock),
    %%timer:sleep(5000),
    exit(normal).

getUniqueId() ->
    %% make_ref().
    random:uniform(100).

send2(Sock, Msg) ->
    gen_tcp:send(Sock, Msg),
    receive
	{tcp, Sock, Data} -> io:format("~nOK - client send <~s> ~n            receive <~p>~n~n~n~n", [Msg, Data])
    after
	1000 -> io:fwrite("ERROR - client send <~p> and didn't receive a response~n", [Msg])
    end,
    ok.

send(Sock, Msg) ->
    gen_tcp:send(Sock, Msg),
    ok.

%% start server
ss() ->
    Port = 8000 + random:uniform(100),
    ss(Port).

ss(Port) ->
    server:init([Port]),
    Port.


%% start new socket for user
new_con(Port) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    Pid = spawn(?MODULE, new_con_loop, [Sock]),
    ok = gen_tcp:controlling_process(Sock, Pid),
    ok = inet:setopts(Sock, [{active, true}]),
    Pid ! ok,
    Sock.

%% process's funtion to notify every message received from the server to the target client
new_con_loop(Sock) ->
    receive ok -> ok end,
    receive
	{tcp, Sock, Data} ->
	    io:format("CLIENT receive >>>  ~p~n", [Data]),
	    self() ! ok,
	    new_con_loop(Sock);
	_ ->
	    io:format("CLIENT receive >> wrong format TCP~n", []),
	    self() ! ok,
	    new_con_loop(Sock)
    end,
    ok.
