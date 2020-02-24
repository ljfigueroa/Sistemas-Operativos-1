-module(puser).
-include("user_interface.hrl").
-compile(export_all).

puser() ->
    Map = #{"Laura" => not_in_game},
    %% Map = #{},
    user_loop(Map).

user_loop(Users) ->
    receive
	{From, {add, User_name, S, P, N}} -> 
	    case maps:find(User_name, Users) of
		{ok, _} ->
		    From ! {self(), add, user_name_in_use},
		    user_loop(Users);
		_ ->
		    User = #user{name = User_name,
				 game = not_in_game,
				 socket = S,
				 pid = P,
				 node = N},
		    From ! {self(), add, user_added, User},
		    user_loop(maps:put(User_name, User, Users))
		end;
	{From, {get, User_name}} ->
	    case maps:find(User_name, Users) of
		{ok, Value} -> From ! {self(), get, Value};
		_           -> From ! {self(), get, user_not_found}
	    end,
	    user_loop(Users)
    end.

get(Pid, User_name) ->
    %% should have Self = self()  to pattern match the recieve? 
    Pid ! {self(), {get, User_name}},
    receive
	{Pid , get, Response} -> Response
    end.

add(Pid, User_name, Socket, P, Node) ->
    Pid ! {self(), {add, User_name, Socket, P, Node}},
    receive
	{Pid, add, user_added, User} -> {user_added_ok, User};
	{Pid, add, user_name_in_use} -> user_name_in_use
    end.



