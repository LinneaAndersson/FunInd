tff(goal, conjecture,
  ![X:nat, Y:nat]:
    (![Y2:nat]: X = p(Y2, X) <=> Y2 = z =>
       s(X) = p(Y, s(X)) <=> Y = z)).
tff(axiom, axiom,
  ![A:nat, B:nat, C:nat]: p(A, p(B, C)) = p(p(A, B), C)).
tff(axiom, axiom, ![X:nat]: p(X, z) = X).
tff(axiom, axiom, ![X:nat, Y:nat]: p(s(X), Y) = p(X, s(Y))).