-module(server).
-import(string, [left/3, trim/1]).
-import(lists, [member/2]).
-import(pbalance, [get_server/1]).
-import(puser, [user/0]).
-import(pgame, [add/2, get/2]).
-import(constants, [get_string/1]).
-include("game_interface.hrl").
-compile(export_all).

println(Msg) ->
    io:fwrite("~p~n", [Msg]).

%%% @doc Initialize the server:
%%% @param Ports. List of ports to be listended by the server.
init(Ports) ->
    register(users, spawn(puser, puser, [])),
    %% register(pb, spawn(?MODULE, pbalance, [])),
    register(games, spawn(pgame, pgame, [])),
    %% register(provider, spawn(?MODULE, provider, [])),
    %% register(pst, spawn(?MODULE, pstat, [])),
    spawn(?MODULE, dispatcher, [Ports]).

%%% @doc Initialize the dispatcher process
%%% @param Ports. List of ports to be transformed to sockets to be listened.
dispatcher(Ports) ->
    [Port | _] = Ports,
    {ok, ListenSock} = gen_tcp:listen(Port, [{active, false}]),
    loop_dispatcher(ListenSock).

%%% @doc Dispatcher process.
%%% @param ListenSock socket listened to.
loop_dispatcher(ListenSock) ->
    {ok, Sock} = gen_tcp:accept(ListenSock),
    Pid = spawn(?MODULE, psocket, [Sock]),
    ok = gen_tcp:controlling_process(Sock, Pid),
    ok = inet:setopts(Sock, [{active, true}]),
    Pid ! ok,
    loop_dispatcher(ListenSock).

%%% @doc
psocket(Sock) ->
    receive ok -> ok end,
    receive
	%% Connect from a client
        {tcp, Sock, Cmd} ->
            case isValidConnectPcomand(Cmd) of
                {ok, UserName} -> pcomand_connect(Sock, create_user(UserName));
                {error, Msg}   -> gen_tcp:send(Sock, Msg),
				  self() ! ok,
				  psocket(Sock)
            end;
        _ -> ok,
	     self() ! ok,
	     psocket(Sock)
    end.

psocket_loop(Sock) ->
    %% io:fwrite("psocket_loop ~n"),
    receive
        {tcp, Sock, Cmd} -> IsValidPcommand = isValidPcommand(Cmd),
                            if
                                IsValidPcommand -> spawn_pcommand(Sock, Cmd);
				true -> gen_tcp:send(Sock, "Invalid pcommand")
                            end;
	_ -> println("psocket_loop no entiende lo que recivio.")
    after
        1000 -> println("@@@@@@@@@@@@@@@ psocket_loop no recivio nada")
		%% exit(kill)
    end,
    psocket_loop(Sock).

pbalance() ->
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
        _ -> {error, constants:get_string(invalid_con_command)}
    end.

isValidPcommand(String) ->
    ValidPcommands = ["LSG", "NEW", "ACC", "PLA", "OBS", "LEA", "BYE"],
    lists:member(String, ValidPcommands).


spawn_pcommand(Server, Cmd) ->
    %% io:format("NEW PCOMAND\n"),
    spawn(?MODULE, pcomando, [Server, Cmd]).

pcomando(Server, Cmd) ->
    Args = [""],
    %% io:fwrite("##  EJECUTANDO PCOMANDO =>  <~p>~n",[Cmd]),
    [Command | Arguments] = string:tokens(Cmd, " "),
    case Command of
	"LSG" ->
	    Response = getAllGames(),
	    Res = io_lib:format("~p", [Response]),
	    %% io:fwrite("LGS response ~p ~n", [Res]),
	    %% send_request(Server, Command, Args, Response);
	    gen_tcp:send(Server, Res);
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

pcomand_connect(Sock, user_added_ok) ->
    gen_tcp:send(Sock, "OK USER :D"),
    psocket_loop(Sock);
pcomand_connect(Sock, user_already_exist) ->
    gen_tcp:send(Sock, constants:get_string(user_already_exist)),
    self() ! ok,
    psocket(Sock);
pcomand_connect(_, _) ->
    io:format("pcomand_connect with wrong arguments~n").

create_user(Name) ->
    User = getUser(Name),
    case User of
        user_not_found -> user_added_ok = addUser(Name);
	_             -> user_already_exist
    end.

send_request(Server, Command, Arguments, Response) ->
    Args = string:concat(Arguments),
    Res = string:concat(Response),
    Post = string:concat(Command, Args, Res),
    gen_tcp:send(Server, Post).

%% puser
addUser(User_name) ->
    puser:add(whereis(users), User_name).

getUser(User_name) ->
    puser:get(whereis(users), User_name).

%% pgame
getGame(Game_id) ->
    pgame:get(whereis(games), Game_id).

getAllGames() ->
    pgame:get_all(whereis(games)).

addGame(Game) ->
    pgame:add(whereis(games), Game).
