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
    user_added_ok = user:add(PUser, User1#user.name),
    user_added_ok = user:add(PUser, User2#user.name),
    user_added_ok = user:add(PUser, UserLongName#user.name),
    %% 3- get games
    "Lauro" = (user:get(PUser, "Lauro"))#user.name,
    user_not_found = user:get(PUser, "some name that doesn't exist"),
    io:format("[ok] tusers\n"),
    ok.
