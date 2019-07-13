-module(user).
-include("user_interface.hrl").
-compile(export_all).

user() ->
    Map = #{"Laura" => not_in_game},
    %% Map = #{},
    user_loop(Map).

user_loop(Users) ->
    receive
	{From, {add, User_name}} ->
	    From ! {self(), user_added},
	    %User = #user{name = User_name},
	    user_loop(maps:put(User_name, not_in_game, Users));
	{From, {get, User_name}} ->
	    case maps:find(User_name, Users) of
            {ok, Value} ->
                User = #user{name = User_name, game_id = Value},
                From ! {self(), {ok, User}};
    		error ->
                From ! {self(), user_not_found}
	    end,
	    user_loop(Users)
    end.

get(Pid, User_name) ->
    Pid ! {self(), {get, User_name}},
    receive
	{_, User} -> User
    end.

add(Pid, User_name) ->
    Pid ! {self(), {add, User_name}},
    receive
	{_, user_added} -> user_added_ok
    end.
