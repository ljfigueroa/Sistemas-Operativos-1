-module(balance_service).
-compile(export_all).

start() ->
    Map = #{},
    main_loop(Map).

main_loop(Map) ->
    receive
	{update, Node, Statistic} ->  
	    NewMap = maps:put(Node, Statistic, Map),
	    %% io:fwrite("NewHeap ~p~n", [NewMap]),
	    main_loop(NewMap);
        {req, Pid} -> 
	    List = maps:to_list(Map),
	    {Node, _ } = min(List),
	    Pid ! {ok, Node},
	    main_loop(Map)
    end.


%% pseudo copy from erlang's list:min implementation
min([H|T]) -> min(T, H).

min([H={N, S} | T], Min={_, MinStatistic}) when S < MinStatistic -> min(T, H);
min([_|T], Min)              -> min(T, Min);
min([],    Min)              -> Min. 


get_server(Pbalance) ->
    io:fwrite("Modulo pbalance y el metodo get_server(~p) ~n", [Pbalance]),
    Pbalance ! {req, self()},
    receive
        {ok, Server} -> {ok, Server}
    end.
