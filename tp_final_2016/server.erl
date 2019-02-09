-module(server).
-import(string, [left/3, trim/1]).
-import(lists, [member/2]).
-import(pbalance, [get_server/1]).
-import(user, [user/0]).
%-import(games,[add/2, get/2]).
-include("game_interface.hrl").
-compile(export_all).

init(Ports) ->
    io:format("1 - spawn dispatcher\n"),
    register(pusers, spawn(user, user, [])),
    register(pb, spawn(?MODULE, pbalance, [])),
    register(pgames, spawn(games, games, [[]])),
    %% register(provider, spawn(?MODULE, provider, [])),
    %% register(pst, spawn(?MODULE, pstat, [])),
    spawn(?MODULE, dispatcher, [Ports]).

dispatcher(Ports) ->
    [Port | _] = Ports,
    {ok, ListenSock} = gen_tcp:listen(Port, [{active, false}]),
    loop_dispatcher(ListenSock).


loop_dispatcher(ListenSock) ->
    {ok, Sock} = gen_tcp:accept(ListenSock),
    Pid = spawn(?MODULE, psocket, [Sock]),
    ok = gen_tcp:controlling_process(Sock, Pid),
    ok = inet:setopts(Sock, [{active, true}]),
    Pid ! ok,
    loop_dispatcher(ListenSock).

psocket(Sock) ->
    receive ok -> ok end,
    receive
	%% Connect from a client
        {tcp, Sock, Cmd} ->
            case isValidConnectPcomand(Cmd) of
                {ok, UserName} ->
		    User = create_user(UserName),
		    pcomand_connect(Sock, User);
                {error, Msg} ->
                    io:fwrite("~p~n", [Msg]),
                    gen_tcp:send(Sock, Msg),
		    self() ! ok,
		    psocket(Sock)
            end;
        _ -> ok,
	     self() ! ok,
	     psocket(Sock)
    end.

psocket_loop(Sock) ->
    io:fwrite("psocket_loop ~n"),
    receive
	{tcp, Sock, Cmd} -> IsValidPcommand = isValidPcommand(Cmd),
			    if
				IsValidPcommand -> 
				    spawn_pcommand(Sock, Cmd);
				true -> 
				    io:fwrite("VALIDN'T ~n"),
				    gen_tcp:send(Sock, "Invalid pcommand")
			    end;
	_ -> io:fwrite("psocket_loop no entiende lo que recivio.~n")
    after
	1000 -> io:fwrite("@@@@@@@@@@@@@@@ psocket_loop no recivio nada ~n"),
		exit(kill)
    end,			       
    psocket_loop(Sock).

pbalance() ->
    io:fwrite("Dentro del proceso pbalance ~n"),
    receive
        {req, Pid} -> Pid ! {ok, "SERVER ;)"}
    end,
    pbalance().

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


isNameAvailable(List, String) -> not(lists:member(String, List)).

isValidConnectPcomand(String) ->
    Ss = string:strip(String),
    case string:str(Ss, "CON") of
        1 -> UserName = string:strip(string:sub_string(Ss, 4)),
	     {ok, UserName};
        _ -> {error, "Error > no es el comando CON"}
    end.

isValidPcommand(String) ->
    ValidPcommands = ["LSG", "NEW", "ACC", "PLA", "OBS", "LEA", "BYE"],
    lists:member(String, ValidPcommands).


spawn_pcommand(Server, Cmd) ->
    io:format("NEW PCOMAND\n"),
    spawn(?MODULE, pcomando, [Server, Cmd]).

pcomando(Server, Cmd) ->
    %% io:fwrite("##  EJECUTANDO PCOMANDO =>  <~p>~n",[Cmd]),
    [Command | Arguments] = string:tokens(Cmd, " "),
    case Command of 
	"LSG" ->
	    Game = games:get(whereis(pgames),123),
	    io:fwrite("game.id <~p> game.state <~p> ~n", [Game#game.id, Game#game.state]),

	    gen_tcp:send(Server,string:concat("Exec command > ", Command));
	"NEW" ->
	    gen_tcp:send(Server,string:concat("Exec command > ", Command));
	"ACC" ->
    	    gen_tcp:send(Server,string:concat("Exec command > ", Command));
	"PLA" ->
	    gen_tcp:send(Server,string:concat("Exec command > ", Command));
	"OBS" ->
	    gen_tcp:send(Server,string:concat("Exec command > ", Command));
	"LEA" -> 
	    gen_tcp:send(Server,string:concat("Exec command > ", Command));
	"BYE" ->
	    gen_tcp:send(Server,string:concat("Exec command > ", Command))
    end.


pcomando(lgs) ->
    ok.

pcomand_connect(Sock, ok) ->
    {ok, Server} = pbalance:get_server(pb),
    io:fwrite("Pbalance dijo <~p>~n", [Server]),
    psocket_loop(Sock);
pcomand_connect(Sock, user_already_exist) ->
    io:format("Name already in use~n"),
    gen_tcp:send(Sock, "Name already in use"),
    self() ! ok,
    psocket(Sock);
pcomand_connect(_, _) ->
    io:format("pcomand_connect with wrong arguments~n").

create_user(Name) ->
    User = user:get(pusers, Name),
    case User of
	user_not_found ->
	    io:fwrite("1 ~n"),
	    user_added_ok = user:add(pusers, Name),
	    io:fwrite("2 ~n"),
	    User2 = user:get(pusers, Name),
	    io:fwrite("aaaaa ~p ~n", [User2]),
	    io:fwrite("se agrego correctamente el usuario ~n"), 
	    ok;
	_ ->
	    io:format("Name already in use ~n"),
	    user_already_exist
    end.
