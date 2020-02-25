-module(puser).
-include("user_interface.hrl").
-compile(export_all).

puser() ->
    Map = #{"Laura" => not_in_game},
    %% Map = #{},
    user_loop(Map).

user_loop(Users) ->
    receive
	{From, test} ->
	    io:fwrite("SIIIIIIIIIIIIIIIII llego test ~n"),
	    From ! {response, true},
	    user_loop(Users);
	{is_name_available, Node, Pid, Name} ->
	    io:fwrite("is_name_available ~n"),
	    case maps:find(Name, Users) of
		{ok, _} ->  Pid ! {response, false};
		_       ->  Pid ! {response, true}
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



isNameAvailable(Users, Name) ->
    case maps:find(Name, Users) of
	{ok, _} -> false;
	_ ->
	    io:fwrite("buscando si ~p esta disponible en los otros nodos ~n", [Name]),
	    Servers = nodes(),
	    Fun = (fun(S) -> {askForName(S, Name)} end),
	    ListOfBoolean = lists:map(Fun, Servers),
	    io:fwrite("lista de booleans ~p ~n", [ListOfBoolean]),
	    lists:foldl(fun(X={B}, Res) -> B andalso Res end, true, ListOfBoolean)
    end.

askForName(Node, Name) ->
    {users, Node} ! {self(), test},
    %% {puser, Node} ! {is_name_available, node(), self(),  Name},
    receive
	{response, Boolean} ->
	    io:fwrite("respuesta de users!!!  ~n"),
	    Boolean;
	_  -> io:fwrite("aks for name recibio algo ~n")
    end.
