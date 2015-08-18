-module(serv_nombres).
-compile(export_all).

-record(proc,{name, pid}).

loop(DataList) ->
  receive
    {add,Name,Pid} -> loop(DataList ++ [#proc{name=Name, pid=Pid}]);
    {update,Original,New} -> 
      OldProc = snd(lists:keysearch(Original, 2, DataList)),
      NewProc = OldProc#proc{name=New},
      NewList = lists:keyreplace(Original, 2, DataList, NewProc),
      loop(NewList);
    {send,Name,Msj} ->
        Proc = snd(lists:keysearch(Name, 2, DataList)),
        Proc#proc.pid ! Msj,
        loop(DataList);
    stop -> ok
    end.

snd({_,Snd}) -> Snd. %Usada para obtener el segundo termino de la 
                     %tupla devuelta por keysearch, que es lo buscado.
proc() -> 
  receive
    stop -> ok;
    X -> io:format("my pid: ~p, Msj : ~p ~n",[self(),X]), proc() 
    end.

init() ->
  %Creo el servidor
  register(serv, spawn(?MODULE,loop,[[]])),
  %Agrego procesos al servidor
  serv ! {add,"proc1",spawn(?MODULE,proc,[])},
  serv ! {add,"proc2",spawn(?MODULE,proc,[])},
  serv ! {add,"proc3",spawn(?MODULE,proc,[])},
  %Envio un msj a un proceso
  serv ! {send,"proc3","hola"},
  %Renombro un proceso
  serv ! {update,"proc3","proc4"},
  %Pruebo si se renombro correctamente
  serv ! {send,"proc4","hola por segunda vez"},
  %Detengo los proc
  serv ! {send,"proc4",stop},
  serv ! {send,"proc2",stop},
  serv ! {send,"proc1",stop},
  %Detengo el servidor
  serv ! stop.
