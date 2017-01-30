fof(axiom, axiom, ![X]: (X=z | X=s(proj1S(X)))).
fof(axiom, axiom, ![X]: proj1S(s(X))=X).
fof(axiom, axiom, ![X]: z!=s(X)).
fof(axiom, axiom, ![Y]: p(z, Y)=Y).
fof(axiom, axiom, ![Y, A]: p(s(A), Y)=s(p(A, Y))).
fof(goal, conjecture,
    ~?[X]:
       ~(p(a, p(b, X))!=p(p(a, b), X)
           | (p(s(a), p(b, X))=p(p(s(a), b), X)
                & p(z, p(b, X))=p(p(z, b), X)))
      | ~?[Y]:
           ~(p(Y, p(a, b))!=p(p(Y, a), b)
               | (p(Y, p(s(a), b))=p(p(Y, s(a)), b)
                    & p(Y, p(z, b))=p(p(Y, z), b)))
      | ~?[Z, X2, X3]:
           ~(p(a, X3)!=p(p(a, X2), Z)
               | (p(s(a), X3)=p(p(s(a), X2), Z) & p(z, X3)=p(p(z, X2), Z)))).
