("772.442216s",[Lemma {lemmaName = "lemma80", lemmaSource = Nothing, hLemmas = ["lemma13","lemma35"], indVar = Just [1], formula = "\n  (count (0) (filterLEq (x P.+ x) y)) ===\n    (count (0) (filterLEq x (filterLEq x y)))"},Lemma {lemmaName = "lemma72", lemmaSource = Nothing, hLemmas = ["Sort.prop_countcount"], indVar = Nothing, formula = "\n  ((count x y) P.+ (count x z)) === (count x (y ++ z))"},Lemma {lemmaName = "lemma58", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (ordered (cons x (nil :: list P.Int))) ===\n    (ordered (nil :: list P.Int))"},Lemma {lemmaName = "lemma47", lemmaSource = Nothing, hLemmas = ["lemma6"], indVar = Nothing, formula = "\n  (qsort (cons x (nil :: list P.Int))) ===\n    (cons x (nil :: list P.Int))"},Lemma {lemmaName = "lemma43", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (qsort (nil :: list P.Int)) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma40", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (filterLEq (x P.+ x) (cons (0) (nil :: list P.Int))) ===\n    (filterLEq x (cons (0) (nil :: list P.Int)))"},Lemma {lemmaName = "lemma37", lemmaSource = Nothing, hLemmas = ["lemma35"], indVar = Nothing, formula = "\n  (filterLEq x (cons y (filterLEq x z))) ===\n    (filterLEq x (cons y z))"},Lemma {lemmaName = "lemma35", lemmaSource = Nothing, hLemmas = [], indVar = Just [1], formula = " (filterLEq x (filterLEq x y)) === (filterLEq x y)"},Lemma {lemmaName = "lemma33", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (filterLEq x (nil :: list P.Int)) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma31", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (filterGT (x P.+ x) (cons (0) (nil :: list P.Int))) ===\n    (filterGT x (cons (0) (nil :: list P.Int)))"},Lemma {lemmaName = "lemma24", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (filterGT x (nil :: list P.Int)) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma22", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (count (0) (cons (x P.+ x) y)) === (count (0) (cons x y))"},Lemma {lemmaName = "lemma16", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (count y (cons x (nil :: list P.Int))) ===\n    (count x (cons y (nil :: list P.Int)))"},Lemma {lemmaName = "lemma15", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (count (1) (cons (0) x)) === (count (1) x)"},Lemma {lemmaName = "lemma14", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (count (0) (cons (1) x)) === (count (0) x)"},Lemma {lemmaName = "lemma13", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " ((1) P.+ (count x y)) === (count x (cons x y))"},Lemma {lemmaName = "lemma10", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (count x (nil :: list P.Int)) === (0)"},Lemma {lemmaName = "lemma9", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (cons y (z ++ x2)) === ((cons y z) ++ x2)"},Lemma {lemmaName = "lemma8", lemmaSource = Nothing, hLemmas = ["Sort.prp_pp"], indVar = Nothing, formula = " ((y ++ z) ++ x2) === (y ++ (z ++ x2))"},Lemma {lemmaName = "lemma7", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " ((nil :: list x) ++ y) === y"},Lemma {lemmaName = "lemma6", lemmaSource = Nothing, hLemmas = [], indVar = Just [0], formula = " (y ++ (nil :: list x)) === y"},Lemma {lemmaName = "lemma2", lemmaSource = Just "Sort.p_tmpNil", hLemmas = [], indVar = Nothing, formula = "\n  (count y (nil :: list P.Int)) ===\n    (count y (qsort (nil :: list P.Int)))"},Lemma {lemmaName = "lemma1", lemmaSource = Just "Sort.prop_countcount", hLemmas = [], indVar = Just [0], formula = "\n  ((count x bs) P.+ (count x cs)) === (count x (bs ++ cs))"},Lemma {lemmaName = "lemma0", lemmaSource = Just "Sort.prp_pp", hLemmas = [], indVar = Just [0], formula = " ((a1 ++ b) ++ c) === (a1 ++ (b ++ c))"}])