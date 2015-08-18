-module(anillo_v2).
-compile(export_all).

proc(NPid) ->
%  io:format("Soy ~p , y recivi: ~p ~n",[self(),NPid]),
  receive
    {msj,N} ->
      Self = self(),
      io:format("Soy el primero con ~p elementos y pid ~p ~n",[N,Self]),
      NPid ! {msj,N,Self},
      proc(NPid);
    {msj,N,FPid} when self() == FPid ->
      io:format("Llege al primero pid ~p ~n",[self()]);
    {msj,N,FPid} ->
      io:format("Nodo pid ~p ~n",[self()]),
      NPid ! {msj,N,FPid}
    end.
 
proc() ->
  receive {next, P} ->
    proc(P)
  end.
anillo_initializer(P,N) ->
  P ! {msj,N}.

list_num(0) -> [];
list_num(N) -> list_num(N-1) ++ [N].

init () ->
  N = 10,
  List_num = list_num(N),
  Pids = lists:map(fun(_) -> spawn(?MODULE,proc,[]) end, List_num),
  NextPids = tl(Pids) ++ [hd(Pids)],
  Pairs = lists:zip(Pids, NextPids),
  lists:map((fun({P1, P2}) -> P1 ! {next, P2} end), Pairs),
  anillo_initializer(hd(Pids),N),
  ok. % Para que no se imprima {msj,N} despues de ejecutar anillo_initializer.
