tff(type, type, list : $tType).
tff(func, type, nil : list).
tff(func, type, cons : $int * list > list).
tff(func, type, head : list > $int).
tff(func, type, tail : list > list).
tff(func, type, a : list).
tff(func, type, x : $int).
tff(func, type, y : list * list > list).
tff(func, type, reverse : list > list).
tff(axiom, axiom,
  ![X:list]: (X = nil | X = cons(head(X), tail(X)))).
tff(axiom, axiom, ![X:$int, X2:list]: head(cons(X, X2)) = X).
tff(axiom, axiom, ![X:$int, X2:list]: tail(cons(X, X2)) = X2).
tff(axiom, axiom, ![X:$int, X2:list]: nil != cons(X, X2)).
tff(axiom, axiom, ![Y:list]: y(nil, Y) = Y).
tff(axiom, axiom,
  ![Y:list, Z:$int, Xs:list]: y(cons(Z, Xs), Y) = cons(Z, y(Xs, Y))).
tff(axiom, axiom, reverse(nil) = nil).
tff(axiom, axiom,
  ![Y:$int, Xs:list]:
    reverse(cons(Y, Xs)) = y(reverse(Xs), cons(Y, nil))).
tff(axiom, axiom, reverse(reverse(a)) = a).
tff(goal, conjecture, reverse(reverse(cons(x, a))) = cons(x, a)).
