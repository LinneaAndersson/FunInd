("268.726589s",[Lemma {lemmaName = "lemma43", lemmaSource = Nothing, hLemmas = ["lemma43"], indVar = Nothing, formula = "\n  (ordered (cons x (insert x y))) ===\n    (ordered (cons x (cons x y)))"},Lemma {lemmaName = "lemma33", lemmaSource = Nothing, hLemmas = ["lemma33"], indVar = Nothing, formula = "\n  (ordered (cons x (cons x y))) === (ordered (cons x y))"},Lemma {lemmaName = "lemma32", lemmaSource = Nothing, hLemmas = ["lemma32"], indVar = Nothing, formula = "\n  (ordered (cons x (nil :: list P.Int))) ===\n    (ordered (nil :: list P.Int))"},Lemma {lemmaName = "lemma30", lemmaSource = Nothing, hLemmas = ["lemma30"], indVar = Nothing, formula = " (isort (cons x y)) === (insert x (isort y))"},Lemma {lemmaName = "lemma28", lemmaSource = Nothing, hLemmas = ["lemma28"], indVar = Nothing, formula = " (isort (nil :: list P.Int)) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma26", lemmaSource = Nothing, hLemmas = ["lemma26"], indVar = Nothing, formula = " (insert x (cons x y)) === (cons x (cons x y))"},Lemma {lemmaName = "lemma25", lemmaSource = Nothing, hLemmas = ["lemma25"], indVar = Nothing, formula = "\n  (insert x (nil :: list P.Int)) === (cons x (nil :: list P.Int))"},Lemma {lemmaName = "lemma24", lemmaSource = Nothing, hLemmas = ["lemma24"], indVar = Nothing, formula = "\n  (count (x P.+ x) (cons (0) y)) === (count (x P.+ x) (cons x y))"},Lemma {lemmaName = "lemma21", lemmaSource = Nothing, hLemmas = ["lemma21"], indVar = Nothing, formula = "\n  (count (0) (cons (x P.+ x) y)) === (count (0) (cons x y))"},Lemma {lemmaName = "lemma20", lemmaSource = Nothing, hLemmas = ["lemma20","lemma15","lemma18"], indVar = Nothing, formula = "\n  (count x (cons (y P.+ x) (nil :: list P.Int))) ===\n    (count y (cons (0) (nil :: list P.Int)))"},Lemma {lemmaName = "lemma18", lemmaSource = Nothing, hLemmas = ["lemma18"], indVar = Nothing, formula = "\n  (count x (cons (x P.+ x) y)) === (count x (cons (0) y))"},Lemma {lemmaName = "lemma15", lemmaSource = Nothing, hLemmas = ["lemma15","lemma9"], indVar = Nothing, formula = "\n  (count y (cons x (nil :: list P.Int))) ===\n    (count x (cons y (nil :: list P.Int)))"},Lemma {lemmaName = "lemma13", lemmaSource = Nothing, hLemmas = ["lemma13"], indVar = Nothing, formula = " (count (1) (cons (0) x)) === (count (1) x)"},Lemma {lemmaName = "lemma12", lemmaSource = Nothing, hLemmas = ["lemma12"], indVar = Nothing, formula = " (count (0) (cons (1) x)) === (count (0) x)"},Lemma {lemmaName = "lemma9", lemmaSource = Nothing, hLemmas = ["lemma9"], indVar = Nothing, formula = " ((1) P.+ (count x y)) === (count x (cons x y))"},Lemma {lemmaName = "lemma8", lemmaSource = Nothing, hLemmas = ["lemma8"], indVar = Nothing, formula = " (count x (nil :: list P.Int)) === (0)"},Lemma {lemmaName = "lemma7", lemmaSource = Nothing, hLemmas = ["lemma7"], indVar = Nothing, formula = " (cons y (z ++ x2)) === ((cons y z) ++ x2)"},Lemma {lemmaName = "lemma5", lemmaSource = Nothing, hLemmas = ["lemma5"], indVar = Nothing, formula = " ((nil :: list x) ++ y) === y"}])