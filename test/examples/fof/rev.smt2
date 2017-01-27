fof(types1, axiom, is_list(nil)).
fof(types2, axiom, ![X, X2]: is_list(cons(X2, X))).
fof(types3, axiom, ![X, X2]: is_list(y(X2, X))).
fof(types4, axiom, ![X]: is_list(reverse(X))).
fof(types5, axiom, is_list(a)).
fof(types6, axiom, ![X]: 'is_$int'(head(X))).
fof(types7, axiom, ![X]: is_list(tail(X))).
fof(types8, axiom, 'is_$int'(x)).
fof(types9, axiom, ?[X]: 'is_$int'(X)).
fof(types10, axiom, ?[X]: is_list(X)).
fof(axiom, axiom,
    ![X]: (~is_list(X) | X=nil | X=cons(head(X), tail(X)))).
fof(axiom, axiom, ![X, X2]: (head(cons(X, X2))=X | ~'is_$int'(X))).
fof(axiom, axiom, ![X, X2]: (tail(cons(X, X2))=X2 | ~is_list(X2))).
fof(axiom, axiom, ![X, X2]: ~nil=cons(X, X2)).
fof(axiom, axiom, ![Y]: (y(nil, Y)=Y | ~is_list(Y))).
fof(axiom, axiom,
    ![Y, Z, Xs]: y(cons(Z, Xs), Y)=cons(Z, y(Xs, Y))).
fof(axiom, axiom, reverse(nil)=nil).
fof(axiom, axiom,
    ![Y, Xs]: reverse(cons(Y, Xs))=y(reverse(Xs), cons(Y, nil))).
fof(axiom, axiom, reverse(reverse(a))=a).
fof(goal, conjecture, reverse(reverse(cons(x, a)))=cons(x, a)).
