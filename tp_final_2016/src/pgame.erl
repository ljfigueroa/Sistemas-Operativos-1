-module(pgame).
-compile(export_all).
-include("game_interface.hrl").
-include("user_interface.hrl").

pgame() -> game_loop(#{}).

game_loop(Games) ->
    receive
	{From, {new, User}} ->
	    Id = getUniqueId(),
	    G = #game{p1=User, id=Id, p2=undefined},
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
	    GID = hd(maps:values(Games)),
	    case getGame(GID#game.id, Games) of 
	%% case getGame(GameId, Games) of 
		{value, G} -> 
		    io:format("joining ~p to game ~p ~n", [G, User]),
		    %% or (G#game.p2#user.name == User#user.name) of
		    case (G#game.p1#user.name == User#user.name) of
			true ->
			    NG = G#game{p2=User},
			    game_loop(maps:put(GameId, NG, Games)),
			    From ! {self(), already_joined};
			false ->
			    case G#game.p2 =/= undefined of
				true -> 
				    From ! {self(), game_full};
				false ->
				    NG = G#game{p2=User},
				    game_loop(maps:put(GameId, NG, Games)),
				    From ! {self(), ok}
			    end
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
	{From, get_all_games} ->
	    From ! {self(), Games},
	    game_loop(Games)
    end.

getUniqueId() ->
    random:uniform(1000).

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

watch(Pid, User, GameId) ->
    Pid ! {self(), {watch, User, GameId}},
    receive
	{Pid, Status} -> Status
    end.
