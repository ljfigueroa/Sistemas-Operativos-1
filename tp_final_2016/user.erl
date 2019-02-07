-module(user).
-compile(export_all).
-include("user_interface.hrl").


user() ->
    Map = #{},
    user_loop(Map).

user_loop(Users) ->
    receive
	{From, {add, User_name}} ->
	    From ! {self(), ok},
	    User = #user{name = User_name},
	    user_loop([User | Users]);
	{From, {get, User_name}} ->
	    case map:find(User_name) of
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
	{Pid, User} -> User
    end.

add(Pid, User_name) ->
    Pid ! {self(), {add, User_name}},
    receive
	{Pid, ok} -> ok
    end.			 
