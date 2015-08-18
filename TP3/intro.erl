-module(intro).
-export([init/0]).

match_test () -> 
  {A,B} = {5,4},
% {A,A} = {5,4},
  {B,A} = {4,5},
  {A,A} = {5,5}.

string_test () -> [
  helloworld == 'helloworld',
  "helloworld" == 'helloworld',
  helloworld == "helloworld",
  [$h,$e,$l,$l,$o,$w,$o,$r,$l,$d] == "helloworld",
  [104,101,108,108,111,119,111,114,108,100] == helloworld,
  [104,101,108,108,111,119,111,114,108,100] == 'helloworld',
  [104,101,108,108,111,119,111,114,108,100] == "helloworld"].

tuple_test (P1,P2) ->
  io:format("El nombre de P1 es ~p y el apellido de P2 es ~p~n",[nombre(P1),apellido(P2)]).

apellido ({persona,{_,_},{_,Apellido}}) -> Apellido.
nombre ({persona,{_,Nombre},{_,_}}) -> Nombre.

filtrar_por_apellido(Personas,Apellido) -> [ nombre(P) || P <- Personas, Apellido == apellido(P)].

init () -> 
  P1 = {persona,{nombre,"Juan"},{apellido, "Gomez"}},
  P2 = {persona,{nombre,"Carlos"},{apellido, "Garcia"}},
  P3 = {persona,{nombre,"Javier"},{apellido, "Garcia"}},
  P4 = {persona,{nombre,"Rolando"},{apellido, "Garcia"}},
  match_test(),
  tuple_test(P1,P2),
  io:format("lista ~p ~n",[string_test()]),
% A = "rol",
  Garcias = filtrar_por_apellido([P4,P3,P2,P1],"Garcia").
