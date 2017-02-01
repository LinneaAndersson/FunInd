fof(types1, axiom, is_list(nil)).
fof(types2, axiom, ![X, X2]: is_list(cons(X2, X))).
fof(types3, axiom, ![X, X2]: is_list(x(X2, X))).
fof(types4, axiom, ![X]: 'is_$int'(head(X))).
fof(types5, axiom, ![X]: is_list(tail(X))).
fof(types6, axiom, is_list(a)).
fof(types7, axiom, is_list(b)).
fof(types8, axiom, is_list(c)).
fof(types9, axiom, ?[X]: 'is_$int'(X)).
fof(types10, axiom, ?[X]: is_list(X)).
fof(axiom, axiom,
    ![X]: (~is_list(X) | X=nil | X=cons(head(X), tail(X)))).
fof(axiom, axiom, ![X, X2]: (head(cons(X, X2))=X | ~'is_$int'(X))).
fof(axiom, axiom, ![X, X2]: (tail(cons(X, X2))=X2 | ~is_list(X2))).
fof(axiom, axiom, ![X, X2]: ~nil=cons(X, X2)).
fof(axiom, axiom, ![Y]: (x(nil, Y)=Y | ~is_list(Y))).
fof(axiom, axiom,
    ![Y, Z, Xs]: x(cons(Z, Xs), Y)=cons(Z, x(Xs, Y))).
fof(goal, conjecture, x(a, x(b, c))=x(x(a, b), c)).
