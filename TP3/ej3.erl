-module(ej3).
-export([init/0, wait/1, cronometro/3,temp/3]).

wait(N) -> 
  receive
  after N -> ok
  end.

cronometro(Fun, Hasta, Periodo) ->
  spawn(ej3, temp, [Fun, Hasta, Periodo]).

temp(Fun, Hasta, Periodo) ->
  if 
    Hasta =< 0 -> 
      self() ! exit;
    true ->
      true
  end,
  receive
    exit -> ok
  after
    Periodo -> 
      Fun(),   
      temp(Fun, Hasta-Periodo, Periodo)
  end.

init() ->
  io:format("Espera de 2s ~n"),
  wait(2000),
  io:format("Fin de la espera, comienza el cronometro. ~n"),
  cronometro(fun () -> io:format("Tick~n") end,60000,5000),
  io:format("Prueba de que el interprete no se bloqueo. ~n"),
  wait(70000). % Espera para que no se interrumpa la ejecucion de cronometro.

