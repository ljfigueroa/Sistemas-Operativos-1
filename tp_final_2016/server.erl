-module(server).
-import(string,[left/3, trim/1]). 
-compile(export_all).   

test(String) ->
    Str1 = "            hello",
    io:fwrite("~p~n", [string:trim(Str1)]),
    Str2 = left(Str1,10,$.), 
    io:fwrite("~p~n",[Str2]).

init(Ports) ->
    io:format("1 - spawn dispatcher\n"),
    spawn(?MODULE, dispatcher, [Ports]).

dispatcher(Ports)->
    [Port| _] = Ports,
    io:format("2 - listen to port\n"),
    {ok,ListenSock} = gen_tcp:listen(Port,[{active,false}]),
    io:format("3 - Started listening to port\n"),
    loop_dispatcher(ListenSock).


loop_dispatcher(ListenSock)->
    io:format("4 - Esperando una conexion \n"),
    {ok,Sock} = gen_tcp:accept(ListenSock),
    io:format("6 - Nueva coneccion\n"),
    ok = inet:setopts(Sock,[{active,true}]),
    Pid = spawn(?MODULE, psocket, [Sock]),
    io:format("7 - spawn psocket\n"),
    ok = gen_tcp:controlling_process(Sock,Pid),
    Pid!ok,
    loop_dispatcher(ListenSock).


psocket(Sock)->
    receive ok -> ok end,
    io:format("IIIIIIIIIIIIIIIIIIIIIIIIIII \n"),
    receive
        %% Connect from a client 
	{tcp,Socket,Cmd} -> 
	    case isConnectPcomand(Cmd) of
		{ok, UserName} ->
		    io:fwrite("New user:<~p>~n",[UserName]);
		{error, Msg} -> Msg			    
	    end,
	    Respuesta = pcomando(Cmd),
	    gen_tcp:send(Socket,Respuesta);
	_ -> ok

    end,
    psocket(Sock).

isValidName(String) -> ok.

isConnectPcomand(String) -> 
    Ss = string:strip(String),
    case string:str(Ss, "CON") of
	1 -> Sss = string:strip(string:sub_string(Ss,4)),
	     {ok, Sss};
	_ -> {error, "Error > no es el comando CON"}
    end.

pcomando(Cmd)->
    io:format("EJECUTANDO PCOMANDO\n"),
    "ERROR no implementado".
