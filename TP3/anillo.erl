-module(anillo).
-export([init/0,proc/1]).
-compile(export_all).

proc(NPid) ->
  receive
    {msj,0} -> ok;
    {msj,N} ->
      io:format("nodo ~p, pid ~p ~n",[N,self()]),
      NPid ! {msj,N-1}
    end.
 
proc() ->
  receive {next, P} ->
    proc(P)
  end.
anillo_initializer(P,N) ->
  P ! {msj, N}.

% Hace una lista de enteros [1..N]
list_num(0) -> [];
list_num(N) -> list_num(N-1) ++ [N].

init () ->
  N = 10,
  List_num = list_num(N),
  Pids = lists:map(fun(_) -> spawn(?MODULE,proc,[]) end, List_num),
  NextPids = tl(Pids) ++ [hd(Pids)],
  Pairs = lists:zip(Pids, NextPids),
  lists:map((fun({P1, P2}) -> P1 ! {next, P2} end), Pairs),
  anillo_initializer(hd(Pids), N).
