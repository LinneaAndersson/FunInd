tff(type, type, nat : $tType).
tff(type, type, sk : $tType).
tff(type, type, sk2 : $tType).
tff(func, type, z : nat).
tff(func, type, s : nat > nat).
tff(func, type, proj1S : nat > nat).
tff(func, type, x : nat).
tff(func, type, y : nat).
tff(func, type, p : nat * nat > nat).
tff(axiom, axiom, ![X:nat]: (X = z | X = s(proj1S(X)))).
tff(axiom, axiom, ![X:nat]: proj1S(s(X)) = X).
tff(axiom, axiom, ![X:nat]: z != s(X)).
tff(axiom, axiom, ![Y:nat]: p(z, Y) = Y).
tff(axiom, axiom, ![Y:nat, A:nat]: p(s(A), Y) = s(p(A, Y))).
tff(goal, conjecture, s(x) = p(y, s(x)) <=> y = z).
tff(axiom, axiom, ![Y:nat]: x = p(Y, x) <=> Y = z).
tff(axiom, axiom,
  ![A:nat, B:nat, C:nat]: p(A, p(B, C)) = p(p(A, B), C)).
tff(axiom, axiom, ![X:nat]: p(X, z) = X).
tff(axiom, axiom, ![X:nat, Y:nat]: p(s(X), Y) = p(X, s(Y))).