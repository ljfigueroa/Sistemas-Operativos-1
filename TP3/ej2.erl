-module(ej2).
-export([init/0]).

operar({cte, Arg}) -> Arg;
operar({OP, Arg1, Arg2}) ->
  case OP of 
    suma -> operar(Arg1) + operar(Arg2);
    resta -> operar(Arg1) - operar(Arg2);
    prod -> operar(Arg1) * operar(Arg2);
    divi -> operar(Arg1) / operar(Arg2)
  end.
    
init() ->
  io:format("operar({cte, 4}) = ~p~n",[operar({cte, 4})]),
  io:format("operar({suma,{cte, 4}, {cte, 5}}) = ~p~n",[operar({suma,{cte, 4}, {cte, 5}})]),
  io:format("operar({resta,{cte, 4}, {cte, 5}}) = ~p~n",[operar({resta,{cte, 4}, {cte, 5}})]),
  io:format("operar({prod,{cte, 4}, {cte, 5}}) = ~p~n",[operar({prod,{cte, 4}, {cte, 5}})]),
  io:format("operar({divi,{cte, 4}, {cte, 5}}) = ~p~n",[operar({divi,{cte, 4}, {cte, 5}})]),
  io:format("operar({divi, {cte, 4}, {prod, {cte, 4}, {cte, 5}}}) =  ~p~n",[operar({divi, {cte, 4}, {prod, {cte, 4}, {cte, 5}}})]).
