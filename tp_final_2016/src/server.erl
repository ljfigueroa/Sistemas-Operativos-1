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
            case pcommand:parse_connect(Message) of
                {ok, Pcommand} ->
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
	    end,
	    psocket_loop(U);
	{pcommand, Message} ->
	    %% io:format("psocket_loop is sending the response ~s ~n",[Message]),
	    gen_tcp:send(Sock, io_lib:format("~s", [Message])), %% "Invalid command");
	    %% get_tcp:send(Sock, Message);
	    psocket_loop(U);
	{pcommand, bye, Message} ->
	    %% io:format("psocket_loop BYE BYE BYE  ~s ~n",[Message]),
	    gen_tcp:send(Sock, io_lib:format("~s", [Message])),
	    gen_tcp:close(Sock); %% don't loop, finsh process here
	_ ->
	    println("psocket_loop no entiende lo que recivio."),
	    psocket_loop(U)
    end. %% only reach by BYE command.

isNameAvailable(List, String) -> not(lists:member(String, List)).

spawn_pcommand(Server, User, Cmd) ->
    %% io:format("NEW PCOMAND\n"),
    {ok, Node} = balance_service:get_server(pbalance),
    spawn(Node, ?MODULE, pcomando, [Server, User, Cmd]).

pcomando(Socket, U,  Cmd=#pcommand{id=lgs}) ->
    %% io:fwrite("EXEC LSG in node ~p ~n", [node()]),
    Response = getAllGames(),
    io:fwrite("EXEC LSG in node ~p ~p ~n", [node(), Response]),
    Res = pcommand:format(ok, Cmd, {Response}),
    U#user.pid ! {pcommand, Res};
    %% gen_tcp:send(Socket, Res);
pcomando(Socket, U, Cmd=#pcommand{id=new}) ->
    %% io:fwrite("EXEC NEW in node ~p ~n", [node()]),
    ok = newGame(U),
    Res = pcommand:format(ok, Cmd, {}),
    U#user.pid ! {pcommand, Res};
pcomando(Socket, U, Cmd=#pcommand{id=acc, game_id=GameId}) ->
    %% The user joining must not be the one who created the game.
    S = joinGame(U, GameId),
    case S of
	{ok, PType, OtherPlayer, Game} ->
	    {Req, Upd} = pcommand:format(ok, Cmd, {PType, U, Game}),
	    %% notify other user if exist
	    notifyOtherPlayer(getOtherPlayer(Game, U), Game, Upd),
	    U#user.pid ! {pcommand, Req};
	{_, Msg} ->
	    {Req, _} = pcommand:format(Msg, Cmd, {}),
	    U#user.pid ! {pcommand, Req}
    end;
pcomando(Socket, U, Cmd=#pcommand{id=pla, game_id=GameId, move=PlayMove}) ->
    S = playGame(U, GameId, PlayMove),
    case S of
	{ok, PType, Game} ->
	    {Req, Upd} = pcommand:format(ok, Cmd, {PType, Game, PlayMove}),
	    %% notify other user if exist
	    notifyOtherPlayer(getOtherPlayer(Game, U), Game, Upd),
	    U#user.pid ! {pcommand, Req};
	{_, Msg} ->
	    {Req, _} = pcommand:format(Msg, Cmd, {}),
	    %% le deberia avisar  Game#game.p1??
	    U#user.pid ! {pcommand, Req}
    end;
pcomando(Socket, U, Cmd=#pcommand{id=obs, game_id=GameId}) ->
    S = watchGame(U, GameId),
    case S of
	{ok, _} -> %% 2nd argumente is Game
	    {Req, _} = pcommand:format(ok, Cmd, {}),
	    U#user.pid ! {pcommand, Req};
	{_, Msg} ->
	    {Req, _} = pcommand:format(Msg, Cmd, {}),
	    U#user.pid ! {pcommand, Req}
    end;
pcomando(Socket, U, Cmd=#pcommand{id=lea, game_id=GameId}) ->
    S = leaveGame(U, GameId),
    case S of
	{ok, _} -> %% 2nd argumente is Game
	    {Req, _} = pcommand:format(ok, Cmd, {}),
	    U#user.pid ! {pcommand, Req};
	{_, Msg} ->
	    {Req, _} = pcommand:format(Msg, Cmd, {}),
	    U#user.pid ! {pcommand, Req}
    end;
pcomando(Socket, U, Cmd=#pcommand{id=bye}) ->
    io:fwrite("EXEC BYE in node ~p ~n", [node()]),
    {ok, LSG} = byeGame(U), %% list of games updated
    io:fwrite("bye for games ~p ~n", [LSG]),
    Fun = fun(G) -> notifyOtherPlayer(getOtherPlayer(G, U), G, pcommand:format(ok, Cmd, {G, U})) end,
    lists:map(Fun, LSG), %% notify all user from every game updated
    U#user.pid ! {pcommand, bye, "OK"};
pcomando(Socket, U, _) ->
    %% this shouldn't happen.
    U#user.pid ! {pcommand, io_lib:format("~s", ["Unsupported pcommand option"])}.


pcomand_connect(Sock, {user_added_ok, User}) ->
    gen_tcp:send(Sock, "OK USER :D"),
    psocket_loop(User);
pcomand_connect(Sock, user_name_in_use) ->
    gen_tcp:send(Sock, "User name already used"),
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

notifyOtherPlayer(undefined, _, _) ->
    ok; %% do nothing if there is no other player in the game
notifyOtherPlayer(User, Game, Msg) ->
    User#user.pid ! {pcommand, Msg},
    %% io:fwrite("notify observers game ~p and observers ~p ~n", [Game, Game#game.obs]),
    Fun = (fun(U) -> U#user.pid ! {pcommand, Msg} end),
    lists:map(Fun, Game#game.obs).


getOtherPlayer(Game=#game{p1=#user{name = Name}}, User=#user{name=Name}) ->
    Game#game.p2;
getOtherPlayer(Game=#game{p2=#user{name = Name}}, User=#user{name=Name}) ->
    Game#game.p1;
getOtherPlayer(Game=#game{p2=undefined}, _) ->
    Game#game.p1;
getOtherPlayer(Game=#game{p1=undefined}, _) ->
    Game#game.p2;
getOtherPlayer(_, _) ->
    undefined.



%% puser
addUser(User_name, Socket, Pid, Node) ->
    puser:add(whereis(users), User_name, Socket, Pid, Node).

getUser(User_name) ->
    puser:get(whereis(users), User_name).

removeUser(User_name) ->
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

playGame(User, GameId, PlayMove) ->
    pgame:play(whereis(games), User, GameId, PlayMove).

watchGame(User, GameId) ->
    pgame:watch(whereis(games), User, GameId).

leaveGame(User, GameId) ->
    pgame:leave(whereis(games), User, GameId).

byeGame(User) ->
    pgame:bye(whereis(games), User).
