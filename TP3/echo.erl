-module(echo).
-compile(export_all).

loop() ->
  receive
    stop -> ok;
    {From, Message} ->
       From ! {self(),Message},
       loop()
    end.

init() ->
  register(echo, spawn(?MODULE,loop,[])),
  %test 
  io:format("My pid : ~p~n",[self()]),
  echo ! {self(), "hola"},
  receive {Pid, Msj} -> io:format("~p : ~p~n",[Pid,Msj]) end,
  echo ! stop.

