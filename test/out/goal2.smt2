tff(axiom, axiom, ![X:nat]: (p(X, z) = X)).
tff(axiom, axiom, ![X:nat, Y:nat]: (p(s(X), Y) = p(X, s(Y)))).
tff(axiom, axiom, ![X:nat, Y:nat]: (X = p(Y, X) <=> Y = z)).
tff(axiom, axiom,
  ![X:nat, Y:nat, Z:nat]: (p(p(X, Y), Z) = p(X, p(Y, Z)))).
tff(axiom, axiom,
  ![X:nat, Y:nat, Z:nat]: (p(Y, X) = p(Z, X) <=> Y = Z)).
tff(axiom, axiom, ![X:nat, Y:nat]: (p(X, X) = p(Y, Y) <=> X = Y)).
tff(goal, conjecture, ![X:nat, Y:nat]: (p(Y, X) = p(X, Y))).