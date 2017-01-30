tff(type, type, nat : $tType).
tff(type, type, sk : $tType).
tff(type, type, sk2 : $tType).
tff(func, type, z : nat).
tff(func, type, s : nat > nat).
tff(func, type, proj1S : nat > nat).
tff(func, type, a : nat).
tff(func, type, b : nat).
tff(func, type, p : nat * nat > nat).
tff(axiom, axiom, ![X:nat]: (X = z | X = s(proj1S(X)))).
tff(axiom, axiom, ![X:nat]: proj1S(s(X)) = X).
tff(axiom, axiom, ![X:nat]: z != s(X)).
tff(axiom, axiom, ![Y:nat]: p(z, Y) = Y).
tff(axiom, axiom, ![Y:nat, A:nat]: p(s(A), Y) = s(p(A, Y))).
tff(goal, conjecture,
  ![X:nat]:
    (p(a, p(b, X)) = p(p(a, b), X) =>
       (p(s(a), p(b, X)) = p(p(s(a), b), X) &
          p(z, p(b, X)) = p(p(z, b), X))) |
    ![Y:nat]:
      (p(Y, p(a, b)) = p(p(Y, a), b) =>
         (p(Y, p(s(a), b)) = p(p(Y, s(a)), b) &
            p(Y, p(z, b)) = p(p(Y, z), b))) |
    ![Z:nat, X2:nat, X3:nat]:
      (p(a, X3) = p(p(a, X2), Z) =>
         (p(s(a), X3) = p(p(s(a), X2), Z) & p(z, X3) = p(p(z, X2), Z)))).
