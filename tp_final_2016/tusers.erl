-module(tusers).
-import(user,[get/2, add/2]).
-include("user_interface.hrl").
-compile(export_all).

start() ->
    %% 1- initialize the user map
    PUser = spawn(user, users, []),
    %% 2- add new users
    User1 = #user{name="Lauro"},
    User2 = #user{name="Juan"},
    UserLongName = #user{name="this game has a really long name"},
    user:add(PUser, User1),
    user:add(PUser, User2),
    user:add(PUser, UserLongName),
    %% 3- get games
    User1 = user:get(PUser, "Lauro"),
    user_not_found = user:get(PUser, "some name that doesn't exist"),
    io:format("[ok] tusers\n"),
    ok.
