("578.452333s",[Lemma {lemmaName = "lemma38", lemmaSource = Nothing, hLemmas = ["lemma29","lemma38"], indVar = Just [0], formula = " (isort (x ++ (isort y))) === (isort (x ++ y))"},Lemma {lemmaName = "lemma29", lemmaSource = Nothing, hLemmas = ["lemma29","lemma31"], indVar = Just [0], formula = " (isort (isort x)) === (isort x)"},Lemma {lemmaName = "lemma50", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (insert Z (cons (count x y) z)) ===\n    (cons Z (cons (count x y) z))"},Lemma {lemmaName = "lemma49", lemmaSource = Nothing, hLemmas = ["lemma26","lemma49"], indVar = Just [1], formula = "\n  (insert x ((insert x y) ++ z)) ===\n    ((insert x (insert x y)) ++ z)"},Lemma {lemmaName = "lemma48", lemmaSource = Nothing, hLemmas = ["lemma26","lemma48"], indVar = Just [0], formula = "\n  (insert y (x ++ (cons y z))) === ((insert y x) ++ (cons y z))"},Lemma {lemmaName = "lemma45", lemmaSource = Nothing, hLemmas = ["ISort.prop_countcount"], indVar = Nothing, formula = "\n  (plus (count x y) (count x z)) === (count x (y ++ z))"},Lemma {lemmaName = "lemma43", lemmaSource = Nothing, hLemmas = [], indVar = Just [1], formula = "\n  (ordered (cons x (insert x y))) ===\n    (ordered (cons x (cons x y)))"},Lemma {lemmaName = "lemma33", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (ordered (cons x (cons x y))) === (ordered (cons x y))"},Lemma {lemmaName = "lemma32", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (ordered (cons x (nil :: list Nat))) ===\n    (ordered (nil :: list Nat))"},Lemma {lemmaName = "lemma31", lemmaSource = Nothing, hLemmas = ["lemma31","lemma27"], indVar = Just [1], formula = " (isort (insert x y)) === (insert x (isort y))"},Lemma {lemmaName = "lemma30", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (isort (cons x y)) === (insert x (isort y))"},Lemma {lemmaName = "lemma28", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (isort (nil :: list Nat)) === (nil :: list Nat)"},Lemma {lemmaName = "lemma27", lemmaSource = Nothing, hLemmas = ["lemma26","lemma27"], indVar = Just [2], formula = "\n  (insert y (insert x z)) === (insert x (insert y z))"},Lemma {lemmaName = "lemma26", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (insert x (cons x y)) === (cons x (cons x y))"},Lemma {lemmaName = "lemma25", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (insert x (nil :: list Nat)) === (cons x (nil :: list Nat))"},Lemma {lemmaName = "lemma21", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (count Z (cons (plus x x) y)) === (count Z (cons x y))"},Lemma {lemmaName = "lemma15", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (count y (cons x (nil :: list Nat))) ===\n    (count x (cons y (nil :: list Nat)))"},Lemma {lemmaName = "lemma13", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (count (S Z) (cons Z x)) === (count (S Z) x)"},Lemma {lemmaName = "lemma12", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (count Z (cons (S Z) x)) === (count Z x)"},Lemma {lemmaName = "lemma10", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " P.True === P.True"},Lemma {lemmaName = "lemma9", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (plus (S Z) (count x y)) === (count x (cons x y))"},Lemma {lemmaName = "lemma8", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (count x (nil :: list Nat)) === Z"},Lemma {lemmaName = "lemma7", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (cons y (z ++ x2)) === ((cons y z) ++ x2)"},Lemma {lemmaName = "lemma6", lemmaSource = Nothing, hLemmas = ["lemma6"], indVar = Just [0], formula = " ((y ++ z) ++ x2) === (y ++ (z ++ x2))"},Lemma {lemmaName = "lemma5", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " ((nil :: list x) ++ y) === y"},Lemma {lemmaName = "lemma4", lemmaSource = Nothing, hLemmas = ["lemma4"], indVar = Just [0], formula = " (y ++ (nil :: list x)) === y"},Lemma {lemmaName = "lemma0", lemmaSource = Just "ISort.prop_countcount", hLemmas = ["lemma0"], indVar = Just [1], formula = "\n  (plus (count x bs) (count x cs)) === (count x (bs ++ cs))"}])