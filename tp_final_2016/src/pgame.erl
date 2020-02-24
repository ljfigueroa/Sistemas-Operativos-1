-module(pgame).
-compile(export_all).
-include("game_interface.hrl").
-include("user_interface.hrl").

pgame() -> game_loop(#{}).

game_loop(Games) ->
    receive
	{From, {new, User}} ->
	    Id = getUniqueId(),
	    G = #game{p1=User, id=Id, p2=undefined, state=[]},
	    From ! {self(), ok},
	    game_loop(maps:put(Id, G, Games));
	{From, {add, G}} ->
	    From ! {self(), ok},
	    game_loop(maps:put(G#game.id, G, Games));
	{From, {get, GameId}} ->
	    case getGame(GameId, Games) of
		{value, Game} -> From ! {self(), Game};
		not_found     -> From ! {self(), not_found}
	    end,
	    game_loop(Games);
	{From, {join, User, GameId}} ->
	    %% GID = hd(maps:values(Games)),
	    %% case getGame(GID#game.id, Games) of
	    case getGame(GameId, Games) of
		{value, G} ->
		    io:format("joining ~p to game ~p ~n", [G, User]),
		    %% or (G#game.p2#user.name == User#user.name) of
		    case (G#game.p1#user.name == User#user.name) of
			true ->
			    NG = G#game{p2=User},
			    From ! {self(), already_joined},
			    game_loop(maps:put(GameId, NG, Games));
			false ->
			    case G#game.p2 =/= undefined of
				true ->
				    From ! {self(), game_full},
				    game_loop(Games);
				false ->
				    NG = G#game{p2=User},
				    From ! {self(), ok},
				    game_loop(maps:put(GameId, NG, Games))
			    end
		    end;
		not_found ->
		    From ! {self(), not_found}
	    end,
	    game_loop(Games);
	%%%% PLAY
	{From, {play, User, GameId, PlayMove}} ->
	    io:format("playing gameid ~p user ~p with play ~p ~n", [GameId, User, PlayMove]),
	    case getGame(GameId, Games) of
		{value, G} ->
		    case playMove(User, G, PlayMove) of
			{ok, NG} ->
			    From ! {self(), ok},
			    game_loop(maps:put(GameId, NG, Games));
			{error, Atom} ->
			    From ! {self(), Atom}
		     end;
		not_found ->
		    From ! {self(), not_found}
	    end,
	    game_loop(Games);
	{From, {watch, User, GameId}} ->
	    case getGame(GameId, Games) of
		{value, G} ->
		    Game = G#game{obs = [User | G#game.obs]},
		    From ! {self(), Game};
		not_found ->
		    From ! {self(), not_found}
	    end,
	    game_loop(Games);
	{From, {leave, User, GameId}} ->
	    case getGame(GameId, Games) of
		{value, G} ->
		    Fun = fun(E) -> E#user.name == User#user.name end,
		    Obs = lists:dropwhile(Fun, G#game.obs),
		    Game = G#game{obs = Obs},
		    From ! {self(), Game};
		not_found ->
		    From ! {self(), game_not_found}
	    end,
	    game_loop(Games);
	{From, {bye, User}} ->
	    Fun = fun(_, Game) -> removePlayer(Game, User) end,
	    UpdateGames = maps:map(Fun, Games),
	    From ! {self(), ok},
	    game_loop(maps:merge(Games, UpdateGames));
	{From, get_all_games} ->
	    From ! {self(), Games},
	    game_loop(Games)
    end.

getUniqueId() ->
    random:uniform(1000).


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
	true ->
	   {error, game_has_finish};
	false ->
	    Fun = fun(X={Player, Move}) -> Move == PlayMove end, %% check if move was used
	    case lists:any(Fun, Game#game.state) of
		true ->
		    %% can't user move (it's already present in #game.state)
		    {error, move_is_not_avaliable};
		false ->
		    true
	    end
    end.

validPlayMove(Game, User, PlayMove) ->
    case isUserTurn(Game, User) of
	true ->
	    isMoveAvailable(Game, PlayMove);
	false ->
	    {error, is_not_user_turn}
    end.

playMove(User, Game, PlayMove) ->
    State =  Game#game.state,
    case validPlayMove(Game, User, PlayMove) of
	true ->
	    {ok, Game#game{state=[{getPlayerType(Game, User), PlayMove} | State]} };
	{error, Msg} ->
	    {error, Msg}
   end.

userIn(User, Game) ->
    Fun = fun(Name) -> Name == User#user.name end,
    case lists:filter(Fun, getPlayersName(Game)) of
	false -> false;
	{value, Value} -> true
    end.

getPlayersName(Game) ->
    getPlayerName(Game#game.p1) ++ getPlayerName(Game#game.p2).

getPlayerName(User=#user{name=Name}) ->
    Name;
getPlayerName(_) ->
    [].

getGame(GameId, Games) ->
    io:format("getting ~p  game ~n", [GameId]),
    case maps:get(GameId, Games, not_found) of
	not_found -> not_found;
	Game      -> {value, Game}
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
	{Pid, Games} -> Games
    end.

add(Pid, Game) ->
    Pid ! {self(), {add, Game}},
    receive
	{Pid, ok} -> ok
    end.

new(Pid, User) ->
    Pid ! {self(), {new, User}},
    receive
	{Pid, ok} -> ok
    end.

join(Pid, User, GameId) ->
    Pid ! {self(), {join, User, GameId}},
    receive
	{Pid, Status} -> Status
    end.

play(Pid, User, GameId, PlayMove) ->
    Pid ! {self(), {play, User, GameId, PlayMove}},
    receive
	{Pid, Status} -> Status
    end.

watch(Pid, User, GameId) ->
    Pid ! {self(), {watch, User, GameId}},
    receive
	{Pid, Status} -> Status
    end.

leave(Pid, User, GameId) ->
    Pid ! {self(), {leave, User, GameId}},
    receive
	{Pid, Status} -> Status
    end.

bye(Pid, User) ->
    Pid ! {self(), {bye, User}},
    receive
	{Pid, Status} -> Status
    end.
