-module(statistic_service).
-compile(export_all).

start()->
    main_loop().

main_loop() ->
    receive
    after
        3000 ->
            {ok, Servers} = get_servers(),
            {Total_Reductions, _} = erlang:statistics(reductions),
            Fun = (fun(S) -> {notify(S, Total_Reductions)} end),
            lists:map(Fun, Servers),
            main_loop()
    end.

%% Return the known server/nodes list.
get_servers() ->
    {ok, [node() | nodes()]}.

%% Notify to Node's pbalance process the current node load status.
notify(Node, Total_Reductions) ->
    %% io:format("NEW Notify from  ~p  to  ~p pbalance ~p reductions ~n", [node(), Node, Total_Reductions]),
    {pbalance, Node} ! {update, node(), Total_Reductions}.
