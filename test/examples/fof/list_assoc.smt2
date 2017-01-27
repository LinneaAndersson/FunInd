fof(types1, axiom, is_list(nil)).
fof(types2, axiom, ![X, X2]: is_list(cons(X2, X))).
fof(types3, axiom, ![X, X2]: is_list(z(X2, X))).
fof(types4, axiom, is_list(a)).
fof(types5, axiom, ![X]: 'is_$int'(head(X))).
fof(types6, axiom, ![X]: is_list(tail(X))).
fof(types7, axiom, is_list(b)).
fof(types8, axiom, is_list(x)).
fof(types9, axiom, 'is_$int'(y)).
fof(types10, axiom, ?[X]: 'is_$int'(X)).
fof(types11, axiom, ?[X]: is_list(X)).
fof(axiom, axiom,
    ![X]: (~is_list(X) | X=nil | X=cons(head(X), tail(X)))).
fof(axiom, axiom, ![X, X2]: (head(cons(X, X2))=X | ~'is_$int'(X))).
fof(axiom, axiom, ![X, X2]: (tail(cons(X, X2))=X2 | ~is_list(X2))).
fof(axiom, axiom, ![X, X2]: ~nil=cons(X, X2)).
fof(axiom, axiom, ![Y]: (z(nil, Y)=Y | ~is_list(Y))).
fof(axiom, axiom,
    ![Y, Z, Xs]: z(cons(Z, Xs), Y)=cons(Z, z(Xs, Y))).
fof(axiom, axiom, z(a, z(b, x))=z(z(a, b), x)).
fof(goal, conjecture,
    z(cons(y, a), z(b, x))=z(z(cons(y, a), b), x)).
