-module(broadcaster).
-compile(export_all).

loop(Subscriptors) ->
  receive
    {subscribe, Pid} -> loop(Subscriptors ++ [Pid]); 
    {unsubscribe, Pid} -> loop(remove(Subscriptors, Pid));
    {broadcast, Msj} -> lists:map(fun(P) -> P ! Msj end, Subscriptors),
                        loop(Subscriptors);
    stop -> ok end.

remove([H|T], P) ->
  case H == P of
    true -> T;
    false -> [H] ++ remove(T,P)
  end. 

proc() ->
  receive
    stop -> ok;
    Msj -> io:format("Self : ~p, msj : ~p ~n",[self(),Msj]), proc()
    end.

init() ->
  %test
  Id = spawn(?MODULE,proc,[]),
  register(broadcaster, spawn(?MODULE,loop,[[]])),
  broadcaster ! {subscribe, spawn(?MODULE,proc,[])},
  broadcaster ! {subscribe, spawn(?MODULE,proc,[])},
  broadcaster ! {subscribe, Id},
  broadcaster ! {broadcast, "Primer broadcast"},
  broadcaster ! {unsubscribe, Id}, % remuevo uno de los suscriptores
  broadcaster ! {broadcast, "Segundo broadcast"},
  broadcaster ! {broadcast, stop}, % detengo los proc
  broadcaster ! stop.
