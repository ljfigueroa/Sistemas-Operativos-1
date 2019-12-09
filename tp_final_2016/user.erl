-module(user).
-include("user_interface.hrl").
-compile(export_all).

users() ->
    Map = #{"Laura" => not_in_game},
    %% Map = #{},
    user_loop(Map).

user_loop(Users) ->
    receive
	{From, {add, User_name}} -> 
	    User = #user{name = User_name, game = not_in_game},
	    user_loop(maps:put(User_name, User, Users)),
	    From ! {self(), user_added},
	    user_loop(Users);
	{From, {get, User_name}} ->
	    io:fwrite("get - ~p - ~p \n", User_name, Users),
	    case maps:find(User_name, Users) of
		{ok, Value} -> From ! {self(), Value};
		error       -> From ! {self(), user_not_found}
	    end,
	    user_loop(Users)
    end.

get(Pid, User_name) ->
    Pid ! {self(), {get, User_name}},
    receive
	{Pid, User} -> User
    end.

add(Pid, User_name) ->
    Pid ! {self(), {add, User_name}},
    receive
	{Pid, user_added} -> user_added_ok
    end.
