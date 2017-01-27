fof(axiom, axiom, ![X]: (X=z | X=s(proj1S(X)))).
fof(axiom, axiom, ![X]: proj1S(s(X))=X).
fof(axiom, axiom, ![X]: z!=s(X)).
fof(axiom, axiom, ![Y]: p(z, Y)=Y).
fof(axiom, axiom, ![Y, A]: p(s(A), Y)=s(p(A, Y))).
fof(goal, conjecture, p(a, p(s(b), x))=p(p(a, s(b)), x)).
fof(axiom, axiom, p(a, p(b, x))=p(p(a, b), x)).
