-module(synch).
-export([testLock/0,testSem/0]).

%internal
-export([f/2,waiter/2]).
-export([waiter_sem/2,sem/2]).

-compile(export_all).

%test
-export([lockDump/0, semDump/1]).

lockDump() ->
  receive
    salir -> 
      ok;
    {lock, Pid} -> 
      Pid ! ok,
      receive
        {unlock, Pid} -> lockDump()
        end 
    end.

lock (L) -> 
  L ! {lock, self()},
  receive ok -> ok end.

unlock (L) -> L ! {unlock, self()}.
createLock () -> spawn(?MODULE,lockDump,[]).
destroyLock (L) -> L ! salir. 

semDump(N) ->
  receive
    salir -> ok;
    v -> semDump(N+1);
    {p,Pid} when N > 0 ->
      Pid ! ok,
      semDump(N-1)
    end.

createSem (N) -> spawn(?MODULE,semDump,[N]).
destroySem (S) -> S ! salir.

semP (S) -> 
  S ! {p,self()},
  receive ok -> ok end.

semV (S) -> S ! v.

f (L,W) -> lock(L),
  %   regioncritica(),
  io:format("uno ~p~n",[self()]),
  io:format("dos ~p~n",[self()]),
  io:format("tre ~p~n",[self()]),
  io:format("cua ~p~n",[self()]),
  unlock(L),
  W!finished.

waiter (L,0)  -> destroyLock(L);
waiter (L,N)  -> receive finished -> waiter(L,N-1) end.

waiter_sem (S,0)  -> destroySem(S);
waiter_sem (S,N)  -> receive finished -> waiter_sem(S,N-1) end.


testLock () -> L = createLock(),
  W=spawn(?MODULE,waiter,[L,5]),
  spawn(?MODULE,f,[L,W]),
  spawn(?MODULE,f,[L,W]),
  spawn(?MODULE,f,[L,W]),
  spawn(?MODULE,f,[L,W]),
  spawn(?MODULE,f,[L,W]),
  receive after 1
   -> ok end,
  ok.
 
sem (S,W) -> 
  semP(S),
  %regioncritica(), bueno, casi....
  io:format("uno ~p~n",[self()]),
  io:format("dos ~p~n",[self()]),
  semV(S),
  W!finished.

testSem () -> S = createSem(2), % a lo sumo dos usando io al mismo tiempo
  W=spawn(?MODULE,waiter_sem,[S,5]),
  spawn(?MODULE,sem,[S,W]),
  spawn(?MODULE,sem,[S,W]),
  spawn(?MODULE,sem,[S,W]),
  spawn(?MODULE,sem,[S,W]),
  spawn(?MODULE,sem,[S,W]).
