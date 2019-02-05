-module(games).
-compile(export_all).
-include("game_interface.hrl").


games(Game_list) ->
    receive
	{From, {add, Game}} ->
	    From ! {self(), ok},
	    games([Game | Game_list]);
	{From, {get, Game_id}} ->
	    Game = #game{id=Game_id, state=[]},
	    From ! {self(), Game},
	    games(Game_list)
    end.

get(Pid, Game_id) ->
    Pid ! {self(), {get, Game_id}},
    receive
	{Pid, Game} -> Game
    end.

add(Pid, Game) ->
    Pid ! {self(), {add, Game}},
    receive
	{Pid, ok} -> ok
    end.			 
