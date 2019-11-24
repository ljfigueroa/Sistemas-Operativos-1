-module(tgames).
-import(games,[get/2, add/2, get_all/1]).
-include("game_interface.hrl").
-compile(export_all).

start() ->
    InitialGames = [],
    PGame = spawn(games, games, [InitialGames]),
    %% 1- the initialized games are ok
    InitialGames = games:get_all(PGame),
    %% 2- add new games
    Game1 = #game{id="1"},
    Game2 = #game{id="2"},
    GameLongName = #game{id="this game has a really long name"},
    games:add(PGame, Game1),
    games:add(PGame, Game2),
    games:add(PGame, GameLongName),
    [GameLongName | [Game2 | [Game1 | []]]] = games:get_all(PGame), %% careful with the list's order    
    %% 3- get games
    Game1 = games:get(PGame, "1"),
    not_found = games:get(PGame, "un id de juego que no esta"),
    io:format("[ok] tgames\n"),
    ok.
