-module(puser).
-include("user_interface.hrl").
-compile(export_all).

puser() ->
    Map = #{"Laura" => not_in_game},
    %% Map = #{},
    user_loop(Map).

user_loop(Users) ->
    receive
	%% Request to check if Name is present in current Users.
	{From, {is_name_available, Name}} ->
	    case maps:find(Name, Users) of
		{ok, _} ->  From ! {response, false};
		_       ->  From ! {response, true}
	    end,
	    user_loop(Users);
	{From, {add, User_name, S, P, N}} -> 
	    case isNameAvailable(Users, User_name) of
		false ->
		    From ! {self(), add, user_name_in_use},
		    user_loop(Users);
		true ->
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


%%% @doc Return true if the Name is not being used in the current or other
%%%      nodes. Otherwise return false.
isNameAvailable(Users, Name) ->
    case maps:find(Name, Users) of
	{ok, _} -> false;
	_ ->
	    Servers = nodes(),
	    Fun = (fun(S) -> {askForName(S, Name)} end),
	    ListOfBoolean = lists:map(Fun, Servers),
	    %% return true if all nodes respond with {true}
	    lists:foldl(fun({Bool}, Acc) -> Bool andalso Acc end, true, ListOfBoolean)
    end.

%%% @doc Ask is the Name is available and wait for the response.
askForName(Node, Name) ->
    {users, Node} ! {self(), {is_name_available, Name}},
    receive
	{response, Boolean} ->  Boolean
    end.
