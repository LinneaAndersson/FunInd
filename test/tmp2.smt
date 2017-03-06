tff(type, type, list : $tType).
tff(type, type, list2 : $tType).
tff(type, type, tree : $tType).
tff(type, type, sk : $tType).
tff(func, type, nil : list).
tff(func, type, cons : tree * list > list).
tff(func, type, head : list > tree).
tff(func, type, tail : list > list).
tff(func, type, nil2 : list2).
tff(func, type, cons2 : sk * list2 > list2).
tff(func, type, head2 : list2 > sk).
tff(func, type, tail2 : list2 > list2).
tff(func, type, leaf : sk > tree).
tff(func, type, proj1Leaf : tree > sk).
tff(func, type, node : tree * tree > tree).
tff(func, type, proj1Node : tree > tree).
tff(func, type, proj2Node : tree > tree).
tff(func, type, x : list * list > list).
tff(func, type, y : tree).
tff(func, type, f1 : list > list2).
tff(func, type, z : list2 * list2 > list2).
tff(func, type, f0 : tree > list2).
tff(axiom, axiom,
  ![X:list]: ((X = nil | X = cons(head(X), tail(X))))).
tff(axiom, axiom, ![X:tree, X2:list]: (head(cons(X, X2)) = X)).
tff(axiom, axiom, ![X:tree, X2:list]: (tail(cons(X, X2)) = X2)).
tff(axiom, axiom, ![X:tree, X2:list]: (nil != cons(X, X2))).
tff(axiom, axiom,
  ![X:list2]: ((X = nil2 | X = cons2(head2(X), tail2(X))))).
tff(axiom, axiom, ![X:sk, X2:list2]: (head2(cons2(X, X2)) = X)).
tff(axiom, axiom, ![X:sk, X2:list2]: (tail2(cons2(X, X2)) = X2)).
tff(axiom, axiom, ![X:sk, X2:list2]: (nil2 != cons2(X, X2))).
tff(axiom, axiom,
  ![X:tree]:
    ((X = leaf(proj1Leaf(X)) | X = node(proj1Node(X), proj2Node(X))))).
tff(axiom, axiom, ![X:sk]: (proj1Leaf(leaf(X)) = X)).
tff(axiom, axiom,
  ![X:tree, X2:tree]: (proj1Node(node(X, X2)) = X)).
tff(axiom, axiom,
  ![X:tree, X2:tree]: (proj2Node(node(X, X2)) = X2)).
tff(axiom, axiom,
  ![X:sk, X2:tree, X3:tree]: (leaf(X) != node(X2, X3))).
tff(axiom, axiom, f1(nil) = nil2).
tff(axiom, axiom,
  ![Ts:list, Z:sk]: (f1(cons(leaf(Z), Ts)) = cons2(Z, f1(Ts)))).
tff(axiom, axiom,
  ![Ts:list, P:tree, Q:tree]:
    (f1(cons(node(P, Q), Ts)) = f1(cons(P, cons(Q, Ts))))).
tff(axiom, axiom, ![Y:list2]: (z(nil2, Y) = Y)).
tff(axiom, axiom,
  ![Y:list2, Z:sk, Xs:list2]:
    (z(cons2(Z, Xs), Y) = cons2(Z, z(Xs, Y)))).
tff(axiom, axiom, ![B:sk]: (f0(leaf(B)) = cons2(B, nil2))).
tff(axiom, axiom,
  ![P:tree, Q:tree]: (f0(node(P, Q)) = z(f0(P), f0(Q)))).
tff(goal, conjecture, f1(cons(y, nil)) = f0(y)).
