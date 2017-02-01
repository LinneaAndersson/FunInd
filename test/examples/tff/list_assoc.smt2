tff(type, type, list : $tType).
tff(func, type, nil : list).
tff(func, type, cons : $int * list > list).
tff(func, type, head : list > $int).
tff(func, type, tail : list > list).
tff(func, type, a : list).
tff(func, type, b : list).
tff(func, type, c : list).
tff(func, type, x : list * list > list).
tff(axiom, axiom,
  ![X:list]: (X = nil | X = cons(head(X), tail(X)))).
tff(axiom, axiom, ![X:$int, X2:list]: head(cons(X, X2)) = X).
tff(axiom, axiom, ![X:$int, X2:list]: tail(cons(X, X2)) = X2).
tff(axiom, axiom, ![X:$int, X2:list]: nil != cons(X, X2)).
tff(axiom, axiom, ![Y:list]: x(nil, Y) = Y).
tff(axiom, axiom,
  ![Y:list, Z:$int, Xs:list]: x(cons(Z, Xs), Y) = cons(Z, x(Xs, Y))).
tff(goal, conjecture, x(a, x(b, c)) = x(x(a, b), c)).
