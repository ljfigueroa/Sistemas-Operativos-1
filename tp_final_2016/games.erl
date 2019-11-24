-module(games).
-compile(export_all).
-include("game_interface.hrl").


games(Game_list) ->
    receive
	{From, {add, Game}} ->
	    From ! {self(), ok},
	    games([Game | Game_list]);
	{From, {get, Game_id}} ->
	    GameEq = fun(G) -> G#game.id == Game_id end,
	    case lists:search(GameEq, Game_list) of
		{value, Game} -> From ! {self(), Game};
		false         -> From ! {self(), not_found}
	    end,
	    games(Game_list);
	{From, get_all_games} ->
	    From ! {self(), Game_list},
	    games(Game_list)
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
	{_, Games} -> Games
    end.

add(Pid, Game) ->
    Pid ! {self(), {add, Game}},
    receive
	{_, ok} -> ok
    end.
