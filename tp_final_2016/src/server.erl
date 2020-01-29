-module(server).
-import(string, [left/3, trim/1]).
-import(lists, [member/2]).
%-import(pbalance, [get_server/1]).
-import(puser, [user/0]).
-import(pgame, [add/2, get/2]).
-import(constants, [get_string/1]).
-import(pcommand, [parse/1]).
-include("user_interface.hrl").
-include("game_interface.hrl").
-include("pcommand_interface.hrl").
-compile(export_all).

println(Msg) ->
    io:fwrite("~p~n", [Msg]).

%%% @doc Initialize the server:
%%% @param Ports. List of ports to be listended by the server.
init(Ports) ->
    register(users, spawn(puser, puser, [])),
    register(pbalance, spawn(balance_service, start, [])),
    register(games, spawn(pgame, pgame, [])),
    %% register(provider, spawn(?MODULE, provider, [])),
    %% register(ps, spawn(?MODULE, pstat , [])),
    register(pstat, spawn(statistic_service, start, [])),
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
        {tcp, Sock, Message} ->
            case pcommand:parse(Message) of
                {ok, con, Pcommand} ->
		    %% Valid CON pcommand
		    pcomand_connect(Sock, create_user(Pcommand#pcommand.name, Sock, self()));
                error ->
		    %% Invalid CON pcommand
		    gen_tcp:send(Sock, "Invalid CON command. Try something like CON Lauro."),
		    self() ! ok,
		    psocket(Sock)
            end;
        _ -> ok,
	     self() ! ok,
	     psocket(Sock)
    end.

psocket_loop(U = #user{socket=Sock}) ->
    %% io:fwrite("psocket_loop ~n"),
    receive
        {tcp, Sock, Message} ->
	    case pcommand:parse(Message) of
		{ok, Pcommand} -> spawn_pcommand(Sock, U, Pcommand);
		error -> gen_tcp:send(Sock, "Invalid command")
	    end;
	{pcommand, Message} ->
	    %% io:format("psocket_loop is sending the response ~p ~n",[Message]),
	    gen_tcp:send(Sock, io_lib:format("~s", [Message])); %% "Invalid command");
	    %% get_tcp:send(Sock, Message);
	_ -> 
	    println("psocket_loop no entiende lo que recivio.")
    after
        1000 ->
	    println("@@@@@@@@@@@@@@@ psocket_loop no recivio nada")
	    %% exit(kill)
    end,
    psocket_loop(U).


isNameAvailable(List, String) -> not(lists:member(String, List)).

spawn_pcommand(Server, User, Cmd) ->
    %% io:format("NEW PCOMAND\n"),
    {ok, Node} = balance_service:get_server(pbalance),
    spawn(Node, ?MODULE, pcomando, [Server, User, Cmd]).

pcomando(Socket, U,  Cmd=#pcommand{id=lgs}) ->
    Response = getAllGames(),
    Res = pcommand:format(ok, Cmd, {Response}),
    U#user.pid ! {pcommand, Res};
    %% gen_tcp:send(Socket, Res);
pcomando(Socket, U, Cmd=#pcommand{id=new}) ->
    ok = newGame(U),
    Res = pcommand:format(ok, Cmd, {}),
    U#user.pid ! {pcommand, Res};
pcomando(Socket, U, Cmd=#pcommand{id=acc, game_id=GameId}) ->
    %% gen_tcp:send(Socket, "Exec command > acc");
    G = getGame(GameId),
    %% The user joining must not be the one who created the game.
    S = joinGame(U, GameId),
    %% How to stop 2 different user joining the same game?
    Res = pcommand:format(S, Cmd, {}),
    %% 
    U#user.pid ! {pcommand, Res};
pcomando(Socket, U, Cmd=#pcommand{id=pla}) ->
    U#user.pid ! {pcommand, io_lib:format("~p", [Cmd#pcommand.id])};
pcomando(Socket, U, Cmd=#pcommand{id=obs}) ->
    U#user.pid ! {pcommand, io_lib:format("~p", [Cmd#pcommand.id])};
pcomando(Socket, U, Cmd=#pcommand{id=lea}) ->
    U#user.pid ! {pcommand, io_lib:format("~p", [Cmd#pcommand.id])};
pcomando(Socket, U, Cmd=#pcommand{id=bye}) ->
    U#user.pid ! {pcommand, io_lib:format("~p", [Cmd#pcommand.id])}; 
pcomando(Socket, U, _) ->
    %% this shouldn't happen.
    U#user.pid ! {pcommand, io_lib:format("~s", ["Unsupported pcommand option"])}.


pcomand_connect(Sock, {user_added_ok, User}) ->
    gen_tcp:send(Sock, "OK USER :D"),
    psocket_loop(User);
pcomand_connect(Sock, user_already_exist) ->
    gen_tcp:send(Sock, constants:get_string(user_already_exist)),
    self() ! ok,
    psocket(Sock);
pcomand_connect(_, _) ->
    io:format("pcomand_connect with wrong arguments~n").

create_user(Name, Socket, Pid) ->
    addUser(Name, Socket, Pid,  node()).

send_request(Server, Command, Arguments, Response) ->
    Args = string:concat(Arguments),
    Res = string:concat(Response),
    Post = string:concat(Command, Args, Res),
    gen_tcp:send(Server, Post).

%% puser
addUser(User_name, Socket, Pid, Node) ->
    puser:add(whereis(users), User_name, Socket, Pid, Node).

getUser(User_name) ->
    puser:get(whereis(users), User_name).

%% pgame
getGame(Game_id) ->
    pgame:get(whereis(games), Game_id).

getAllGames() ->
    pgame:get_all(whereis(games)).

addGame(Game) ->
    pgame:add(whereis(games), Game).

newGame(Game) ->
    pgame:new(whereis(games), Game).

joinGame(User, GameId) ->
    pgame:join(whereis(games), User, GameId).
