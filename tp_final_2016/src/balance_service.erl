-module(balance_service).
-compile(export_all).

start() ->
    Map = #{},
    main_loop(Map).

main_loop(Map) ->
    receive
	{update, Node, Statistic} ->  
	    NewMap = maps:put(Node, Statistic, Map),
	    %% io:fwrite("NewHeap ~p from ~p with statistic ~p ~n", [NewMap, Node, Statistic]),
	    main_loop(NewMap);
        {req, Pid} -> 
	    List = maps:to_list(Map),
	    {Node, _ } = min_statistic(List),
	    Pid ! {ok, Node},
	    main_loop(Map)
    end.


%% pseudo copy from erlang's list:min implementation
min_statistic([H|T]) -> min_aux(T, H).

min_aux([H={_, S} | T], {_, MinStatistic}) when S < MinStatistic -> min_aux(T, H);
min_aux([_|T], Min)                                              -> min_aux(T, Min);
min_aux([],    Min)                                              -> Min.


get_server(Pbalance) ->
    io:fwrite("Modulo pbalance y el metodo get_server(~p) ~n", [Pbalance]),
    Pbalance ! {req, self()},
    receive
        {ok, Server} -> {ok, Server}
    end.
