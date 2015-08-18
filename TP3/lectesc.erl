-module(lectesc).
-export([init/0]).
-compile(export_all).

bufferController (Nitem,Max,L) ->
  receive 
    {quieroleer,Pid} when Nitem > 0 ->
      Pid ! lee,
      receive {yalei, Pid} ->
        %simulo la lectura del buffer L
        [_|T] = L,
        bufferController(Nitem-1,Max,T)
        end;
    {quieroescribir, Pid} when Nitem < Max ->
      Pid ! escribe,
      receive {yaescribi, Pid} ->
        bufferController(Nitem+1,Max,L++[Nitem])
        end;
    stop -> ok
    end.

leer () ->
  io:format("Leyendo.. ~p~n",[self()]),
  receive
  after 500 -> ok end. 
escribir () -> 
  io:format("Escribiendo..~p~n",[self()]),
  receive
  after 500 -> ok end. 

lector (Buffer) ->
  Buffer ! {quieroleer, self()},
  receive lee -> leer() end,
  io:format("Termine de leer.. ~p~n",[self()]),
  Buffer ! {yalei, self()},
  lector(Buffer).

escritor (Buffer) ->
  Buffer ! {quieroescribir, self()},
  receive escribe -> escribir() end,
  io:format("Termine de escribir.. ~p~n",[self()]),
  Buffer ! {yaescribi, self()},
  escritor(Buffer).

init () ->
  Buffer = spawn(?MODULE,bufferController,[0,10,[]]),
  spawn(?MODULE,lector,[Buffer]),
  spawn(?MODULE,lector,[Buffer]),
  spawn(?MODULE,lector,[Buffer]),
  spawn(?MODULE,escritor,[Buffer]),
  spawn(?MODULE,escritor,[Buffer]),
  spawn(?MODULE,escritor,[Buffer]),
  spawn(?MODULE,escritor,[Buffer]),
  receive
  after 5000 -> Buffer ! stop end,
  ok.

