tff(type, type, list : $tType).
tff(func, type, nil : list).
tff(func, type, cons : $int * list > list).
tff(func, type, head : list > $int).
tff(func, type, tail : list > list).
tff(func, type, a : list).
tff(func, type, b : list).
tff(func, type, x : list).
tff(func, type, y : $int).
tff(func, type, z : list * list > list).
tff(axiom, axiom,
  ![X:list]: (X = nil | X = cons(head(X), tail(X)))).
tff(axiom, axiom, ![X:$int, X2:list]: head(cons(X, X2)) = X).
tff(axiom, axiom, ![X:$int, X2:list]: tail(cons(X, X2)) = X2).
tff(axiom, axiom, ![X:$int, X2:list]: nil != cons(X, X2)).
tff(axiom, axiom, ![Y:list]: z(nil, Y) = Y).
tff(axiom, axiom,
  ![Y:list, Z:$int, Xs:list]: z(cons(Z, Xs), Y) = cons(Z, z(Xs, Y))).
tff(axiom, axiom, z(a, z(b, x)) = z(z(a, b), x)).
tff(goal, conjecture,
  z(cons(y, a), z(b, x)) = z(z(cons(y, a), b), x)).
