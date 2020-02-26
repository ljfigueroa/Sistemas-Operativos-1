-module(pgame).
-compile(export_all).
-include("game_interface.hrl").
-include("user_interface.hrl").

pgame() -> game_loop(#{}).

game_loop(Games) ->
    receive
	{From, {new, User}} ->
	    Id = getUniqueId(),
	    %% io:fwrite("new game in node ~p ~n", [node()]),
	    G = #game{p1=User, id=Id, p2=undefined, state=[], obs=[]},
	    From ! {self(), new, ok},
	    game_loop(maps:put(Id, G, Games));
	{From, {add, G}} ->
	    From ! {self(), add,  ok},
	    game_loop(maps:put(G#game.id, G, Games));
	{From, {get, GameId}} ->
	    case getGame(GameId, Games) of
		{value, Game} -> From ! {self(), Game};
		not_found     -> From ! {self(), not_found}
	    end,
	    game_loop(Games);
	{From, {join, User, GameId}} ->
	    case getGame(GameId, Games) of
		{value, G} ->
		    P1 = G#game.p1,
		    P2 = G#game.p2,
		    %% io:format("node ~p joining ~p to game ~p ~n", [node(), G, User]),
		    case isUserInGame(G, User) of
			true ->
			    From ! {self(), join, {error, already_joined}},
			    game_loop(Games);
			false ->
			    case P1 =/= undefined andalso P2 =/= undefined of
				true ->
				    From ! {self(), join, {error, game_full}},
				    game_loop(Games);
				false ->
				    %% game has an empty player spot
				    case P1 =/= undefined of
					true -> %% p1 is occupied
					    NG = G#game{p2=User},
					    From ! {self(), join, {ok, p2, P1, NG}},
					    game_loop(updateGame(NG, Games));
					false -> %% p2 is ocupied
					    NG = G#game{p1=User},
					    From ! {self(), join, {ok, p1, P2, NG}},
					    game_loop(updateGame(NG, Games))
				    end
			    end
		    end;
		not_found ->
		    From ! {self(), join, {error, game_not_found}}
	    end,
	    game_loop(Games);
        %%%% PLAY
	{From, {play, User, GameId, PlayMove}} ->
	    %% io:format("playing gameid ~p user ~p with play ~p ~n", [GameId, User, PlayMove]),
	    case getGame(GameId, Games) of
		{value, G} ->
		    case playMove(User, G, PlayMove) of
			{ok, NG} ->
			    From ! {self(), {ok, getPlayerType(G, User), NG}},
			    game_loop(updateGame(NG, Games));
			{error, Atom} ->
			    From ! {self(), {error, Atom}}
		end;
		not_found ->
		    From ! {self(), {error, game_not_found}}
	    end,
	    game_loop(Games);
	{From, {watch, User, GameId}} ->
	    case getGame(GameId, Games) of
		{value, G} ->
		    Game = G#game{obs = [User | G#game.obs]},
		    From ! {self(), watch, {ok, Game}},
		    game_loop(updateGame(Game, Games));
		not_found ->
		    From ! {self(), watch, {error, not_found}}
	    end,
	    game_loop(Games);
	{From, {leave, User, GameId}} ->
	    case getGame(GameId, Games) of
		{value, G} ->
		    Fun = fun(E) -> E#user.name == User#user.name end,
		    Obs = lists:dropwhile(Fun, G#game.obs),
		    Game = G#game{obs = Obs},
		    From ! {self(), leave, {ok, Game}},
		    game_loop(updateGame(Game, Games));
		not_found ->
		    From ! {self(), leave, {error, game_not_found}}
	    end,
	    game_loop(Games);
	{From, {bye, User}} ->
	    {UpdatedGames, GamesWithUser} = removeUserFromGames(Games, User),
	    RemoteGamesWithUser = removeUserFromAllRemoteGames(User),
	    From ! {self(), bye, {ok, GamesWithUser ++ RemoteGamesWithUser}},
	    game_loop(UpdatedGames);
	{From, {remove_user_from_games, User}} ->
	    io:fwrite("remove_user_from_games ~p in node ~p ~n", [User#user.name, node()]),
	    From ! {response, remove_user_from_games, getGamesWithUser(Games, User)},
	    Fun = fun(_, Game) -> removePlayer(Game, User) end,
	    UpdateGames = maps:map(Fun, Games), %% delete user from all the games.
	    game_loop(maps:merge(Games, UpdateGames));
	    %%game_loop(Games);
	{From, {update_game, Game}} ->
	    %% io:fwrite("updating ~p game in node ~p ~n", [Game, node()]),
	    Game2 = maps:get(Game#game.id, Games, not_found),
	    %% io:fwrite("Game2 =  ~p ~n", [Game2]),
	    case maps:get(Game#game.id, Games, not_found) of
		not_found -> game_loop(Games);
		OldGame ->
		    %% io:fwrite("update_game gameid ~p in node ~p ~n", [Game#game.id, node()]),
		    From ! {response, update_game, ok},
		    game_loop(maps:put(Game#game.id, Game, Games))
	    end;
	{From, {get_current_game, GameId}} ->
	    %% io:fwrite("get_current_game gameid ~p in node ~p ~n", [GameId, node()]),
	    From ! {response, get_current_game, maps:get(GameId, Games, not_found)},
	    game_loop(Games);
	{From, {get_current_games}} ->
	    %% io:fwrite("get_current_games from games in node ~p ~n", [node()]),
	    From ! {response, maps:values(Games)},
	    game_loop(Games);
	{From, get_all_games} ->
	    %% io:fwrite("get_all_games from node ~p ~n", [node()]),
	    RGS = getAllRemoteGames(),
	    GS = maps:values(Games),
	    From ! {self(), get_all_games, GS ++ RGS},
	    game_loop(Games)
    end.

getUniqueId() ->
    random:uniform(1000).

getAllRemoteGames() ->
    Servers = nodes(),
    Fun = (fun(S) -> getRemoteGames(S) end),
    LGS = lists:map(Fun, Servers), %% list of list of games
    %% io:fwrite("list of games retrieved from all the servers ~p ~n", [LGS]),
    %% merge de list of list
    lists:foldl(fun(LS, Acc) -> LS ++ Acc end, [], LGS).

getRemoteGames(Node) ->
    %% io:fwrite("Retrieve game list from ~p", [Node]),
    {games, Node} ! {self(), {get_current_games}},
    receive
	{response, GameList} ->  GameList
    end.

getGamesWithUser(Games, User) ->
    Fun = fun(_, Game) -> isUserInGame(Game, User) end,
    GamesWithUser = maps:filter(Fun, Games),
    maps:values(GamesWithUser).

removeUserFromGames(Games, User) ->
    Fun = fun(_, Game) -> isUserInGame(Game, User) end,
    GamesWithUser = maps:filter(Fun, Games),
    case maps:size(GamesWithUser) == 0 of %% check if current games has 
	true -> {Games, []};
	false ->
	    Fun2 = fun(_, Game) -> removePlayer(Game, User) end,
	    UpdateGames = maps:map(Fun2, GamesWithUser), %% delete user from all the games.
	    {maps:merge(Games, UpdateGames), maps:values(UpdateGames)}
    end.

removeUserFromAllRemoteGames(User) ->
    Servers = nodes(),
    Fun = (fun(S) -> removeGameRemoteUser(S, User) end),
    List = lists:map(Fun, Servers), %% list of list of games
    %% merge de list of list
    lists:foldl(fun(LS, Acc) -> LS ++ Acc end, [], List).

removeGameRemoteUser(Node, User) ->
    {games, Node} ! {self(), {remove_user_from_games, User}},
    receive
	{response, remove_user_from_games, GameList} -> GameList
    end.


removePlayer(Game=#game{p1=#user{name = Name}}, User=#user{name=Name}) ->
    Game#game{p1 = undefined};
removePlayer(Game=#game{p2=#user{name = Name}}, User=#user{name=Name}) ->
    Game#game{p2 = undefined};
removePlayer(Game, _) ->
    Game.

getPlayerType(Game=#game{p1=#user{name = Name}}, User=#user{name=Name}) ->
    p1;
getPlayerType(Game=#game{p2=#user{name = Name}}, User=#user{name=Name}) ->
    p2;
getPlayerType(Game, _) ->
    undefined.

isUserInGame(Game, User) ->
    getPlayerType(Game, User) =/= undefined.


isUserTurnAux(_, [])->
    true;
isUserTurnAux(T, [{T, _} | _]) ->
    false;
isUserTurnAux(_, _) ->
    true.

isUserTurn(Game, User) ->
    Ptype = getPlayerType(Game, User),
    State = Game#game.state,
    isUserTurnAux(Ptype, State).


isMoveAvailable(Game, PlayMove) ->
    case length(Game#game.state) == 9 of
	true -> {error, game_has_finish};
	false ->
	    Fun = fun(X={Player, Move}) -> Move == PlayMove end, %% check if move was used
	    case lists:any(Fun, Game#game.state) of
		%% can't user move (it's already present in #game.state)
		true  -> {error, move_is_not_avaliable};
		false ->  true
	    end
    end.

validPlayMove(Game, User, PlayMove) ->
    case isUserTurn(Game, User) of
	true  -> isMoveAvailable(Game, PlayMove);
	false -> {error, is_not_user_turn}
    end.


%% PlayMove < 0 User leave game
%% PlayMove = 0 User accepts game
%% 0 < PlayMove < 10 User make a move
%% PlayMove > 9 User makes an invalid move
playMove(User, Game, PlayMove) when PlayMove < 0 ->
    case isUserInGame(Game,User) of
	true  -> {ok, removePlayer(Game, User)};
	false -> {error, user_is_not_in_game}
    end;
playMove(_, _, PlayMove) when PlayMove == 0 ->
    {error, can_not_join_use_instead_ACC_command};
playMove(User, Game, PlayMove) when PlayMove > 0 andalso PlayMove < 10 ->
    case isUserInGame(Game,User) of
	false -> {error, user_is_not_in_game} ;
        true ->
	    State =  Game#game.state,
	    case validPlayMove(Game, User, PlayMove) of
		true -> {ok, Game#game{state=[{getPlayerType(Game, User), PlayMove} | State]}};
		{error, Msg} -> {error, Msg}
	    end
    end;
playMove(_, _, _) ->
    {error, available_moves_are_from_1_to_9}.

userIn(User, Game) ->
    Fun = fun(Name) -> Name == User#user.name end,
    case lists:filter(Fun, getPlayersName(Game)) of
	false          -> false;
	{value, Value} -> true
    end.

getPlayersName(Game) ->
    getPlayerName(Game#game.p1) ++ getPlayerName(Game#game.p2).

getPlayerName(User=#user{name=Name}) ->
    Name;
getPlayerName(_) ->
    [].


updateGame(Game, Games) ->
    %% io:format("getting ~p  game ~n", [Game#game.id]),
    case maps:get(Game#game.id, Games, not_found) of
	not_found ->
	    Servers = nodes(),
	    Fun = (fun(S) -> updateRemoteGame(S, Game) end),
	    lists:map(Fun, Servers), %% list of list of games
	    %% io:fwrite("send update to all servers ~n"),
	    %% merge de list of list
	    Games;
	OldGame -> %% old state of the game being updated
	    maps:put(Game#game.id, Game, Games)
    end.


updateRemoteGame(Node, Game) ->
    %% io:fwrite("send gane update ~p ~n", [Game]),
    {games, Node} ! {self(), {update_game, Game}}.



getGame(GameId, Games) ->
    %% io:format("getting ~p  game ~n", [GameId]),
    case maps:get(GameId, Games, not_found) of
	not_found ->
	    Servers = nodes(),
	    Fun = (fun(S) -> getRemoteGame(S, GameId) end),
	    LGS = lists:map(Fun, Servers), %% list of list of games
	    %% io:fwrite("list of games retrieved from all the servers ~p ~n", [LGS]),
	    %% merge de list of list
	    R = lists:foldl(fun(G, Result) -> case G =/= not_found of
					      true -> G;
					      false -> Result
					  end
			end, not_found, LGS),
	    %% io:fwrite("R =  ~p ~n", [R]),
	    R;
	Game ->
	    {value, Game}
    end.

getRemoteGame(Node, GameId) ->
    %% io:fwrite("Retrieve game_ID from ~p", [Node]),
    {games, Node} ! {self(), {get_current_game, GameId}},
    receive
	{response, get_current_game, not_found} -> not_found;
	{response, get_current_game, Game} -> {value, Game}
    end.

get(Pid, Game_id) ->
    Pid ! {self(), {get, Game_id}},
    receive
        {Pid, Game} -> Game;
	{Pid, not_found} -> not_found
    end.

get_all(Pid) ->
    Pid ! {self(), get_all_games},
    receive
	{Pid, get_all_games, Games} -> Games
    end.

add(Pid, Game) ->
    Pid ! {self(), {add, Game}},
    receive
	{Pid, add, S} -> S
    end.

new(Pid, User) ->
    Pid ! {self(), {new, User}},
    receive
	{Pid, new, S} -> S
    end.

join(Pid, User, GameId) ->
    Pid ! {self(), {join, User, GameId}},
    receive
	{Pid, join, Status} -> Status
    end.

play(Pid, User, GameId, PlayMove) ->
    Pid ! {self(), {play, User, GameId, PlayMove}},
    receive
	{Pid, Status} -> Status
    end.

watch(Pid, User, GameId) ->
    Pid ! {self(), {watch, User, GameId}},
    receive
	{Pid, watch, Status} -> Status
    end.

leave(Pid, User, GameId) ->
    Pid ! {self(), {leave, User, GameId}},
    receive
	{Pid, leave, Status} -> Status
    end.

bye(Pid, User) ->
    Pid ! {self(), {bye, User}},
    receive
	{Pid, bye, Status} -> Status
    end.
