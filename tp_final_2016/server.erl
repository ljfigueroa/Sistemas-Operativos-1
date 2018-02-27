-module(server).
-compile(export_all).   

init(Ports) ->
    io:format("spawn dispatcher\n"),
    spawn(?MODULE, dispatcher, [Ports]).

dispatcher(Ports)->
    [Port| _] = Ports,
    {ok,ListenSock} = gen_tcp:listen(Port,[{active,false}]),
    {ok,Sock} = gen_tcp:accept(ListenSock),
    loop_dispatcher(ListenSock, Sock).


loop_dispatcher(ListenSock, Sock)->
    Pid = spawn(?MODULE,psocket,[Sock]),
    io:format("spawn psocket\n"),
    ok = gen_tcp:controlling_process(Sock,Pid),
    Pid!ok,
    loop_dispatcher(ListenSock, Sock).


psocket(Sock)->
    %% receive ok -> ok end,
    ok = inet:setopts(Sock,[{active,true}]),
    receive
        {tcp,Socket,Cmd} ->
            Respuesta = pcomando(Cmd),
            gen_tcp:send(Socket,Respuesta)
    end,
    psocket(Sock).


pcomando(Cmd)->
    "ERROR no implementado".
