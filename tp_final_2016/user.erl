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
	    From ! {self(), ok},
	    User = #user{name = User_name},
	    user_loop(maps:put(User_name, not_found, Users));
	{From, {get, User_name}} ->
	    case maps:find(User_name, Users) of
		{ok, Value} ->
		    From ! {self(), {ok, Value}};
		error ->
		    From ! {self(), not_found}
	    end,
	    user_loop(Users)
    end.

get(Pid, User_name) ->
    io:fwrite("USER GET ~n"),
    Pid ! {self(), {get, User_name}},
    receive
	{AnotherPid, User} -> self() ! User
    end.

add(Pid, User_name) ->
    Pid ! {self(), {add, User_name}},
    receive
	{_, ok} -> ok
    end.			 
