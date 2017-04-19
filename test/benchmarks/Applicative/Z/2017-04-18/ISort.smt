("478.581095s",[Lemma {lemmaName = "lemma34", lemmaSource = Nothing, hLemmas = ["lemma34","ISort.prop_ISortSorts"], indVar = Nothing, formula = " (ordered (isort x)) === (ordered (nil :: list P.Int))"},Lemma {lemmaName = "lemma29", lemmaSource = Nothing, hLemmas = ["lemma31"], indVar = Just [1], formula = " (isort (isort x)) === (isort x)"},Lemma {lemmaName = "lemma1", lemmaSource = Just "ISort.prop_ISortSorts", hLemmas = ["lemma36"], indVar = Just [1], formula = " ordered (isort xs)"},Lemma {lemmaName = "lemma50", lemmaSource = Nothing, hLemmas = ["lemma10","lemma50"], indVar = Nothing, formula = "\n  (insert (0) (cons (count x y) z)) ===\n    (cons (0) (cons (count x y) z))"},Lemma {lemmaName = "lemma45", lemmaSource = Nothing, hLemmas = ["lemma45","ISort.prop_countcount"], indVar = Nothing, formula = "\n  ((count x y) P.+ (count x z)) === (count x (y ++ z))"},Lemma {lemmaName = "lemma43", lemmaSource = Nothing, hLemmas = ["lemma36","lemma43"], indVar = Nothing, formula = "\n  (ordered (cons x (insert x y))) ===\n    (ordered (cons x (cons x y)))"},Lemma {lemmaName = "lemma36", lemmaSource = Nothing, hLemmas = [], indVar = Just [0], formula = " (ordered (insert x y)) === (ordered y)"},Lemma {lemmaName = "lemma33", lemmaSource = Nothing, hLemmas = ["lemma33"], indVar = Nothing, formula = "\n  (ordered (cons x (cons x y))) === (ordered (cons x y))"},Lemma {lemmaName = "lemma32", lemmaSource = Nothing, hLemmas = ["lemma32"], indVar = Nothing, formula = "\n  (ordered (cons x (nil :: list P.Int))) ===\n    (ordered (nil :: list P.Int))"},Lemma {lemmaName = "lemma31", lemmaSource = Nothing, hLemmas = ["lemma27"], indVar = Just [0], formula = " (isort (insert x y)) === (insert x (isort y))"},Lemma {lemmaName = "lemma30", lemmaSource = Nothing, hLemmas = ["lemma30"], indVar = Nothing, formula = " (isort (cons x y)) === (insert x (isort y))"},Lemma {lemmaName = "lemma28", lemmaSource = Nothing, hLemmas = ["lemma28"], indVar = Nothing, formula = " (isort (nil :: list P.Int)) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma27", lemmaSource = Nothing, hLemmas = [], indVar = Just [0], formula = "\n  (insert y (insert x z)) === (insert x (insert y z))"},Lemma {lemmaName = "lemma26", lemmaSource = Nothing, hLemmas = ["lemma26"], indVar = Nothing, formula = " (insert x (cons x y)) === (cons x (cons x y))"},Lemma {lemmaName = "lemma25", lemmaSource = Nothing, hLemmas = ["lemma25"], indVar = Nothing, formula = "\n  (insert x (nil :: list P.Int)) === (cons x (nil :: list P.Int))"},Lemma {lemmaName = "lemma24", lemmaSource = Nothing, hLemmas = ["lemma24"], indVar = Nothing, formula = "\n  (count (x P.+ x) (cons (0) y)) === (count (x P.+ x) (cons x y))"},Lemma {lemmaName = "lemma21", lemmaSource = Nothing, hLemmas = ["lemma21"], indVar = Nothing, formula = "\n  (count (0) (cons (x P.+ x) y)) === (count (0) (cons x y))"},Lemma {lemmaName = "lemma20", lemmaSource = Nothing, hLemmas = ["lemma20","lemma15","lemma10"], indVar = Nothing, formula = "\n  (count x (cons (y P.+ x) (nil :: list P.Int))) ===\n    (count y (cons (0) (nil :: list P.Int)))"},Lemma {lemmaName = "lemma18", lemmaSource = Nothing, hLemmas = ["lemma18"], indVar = Nothing, formula = "\n  (count x (cons (x P.+ x) y)) === (count x (cons (0) y))"},Lemma {lemmaName = "lemma15", lemmaSource = Nothing, hLemmas = ["lemma15","lemma10","lemma9"], indVar = Nothing, formula = "\n  (count y (cons x (nil :: list P.Int))) ===\n    (count x (cons y (nil :: list P.Int)))"},Lemma {lemmaName = "lemma13", lemmaSource = Nothing, hLemmas = ["lemma13"], indVar = Nothing, formula = " (count (1) (cons (0) x)) === (count (1) x)"},Lemma {lemmaName = "lemma12", lemmaSource = Nothing, hLemmas = ["lemma12"], indVar = Nothing, formula = " (count (0) (cons (1) x)) === (count (0) x)"},Lemma {lemmaName = "lemma10", lemmaSource = Nothing, hLemmas = [], indVar = Just [0], formula = " ((0) P.<= (count x y)) === P.True"},Lemma {lemmaName = "lemma9", lemmaSource = Nothing, hLemmas = ["lemma9"], indVar = Nothing, formula = " ((1) P.+ (count x y)) === (count x (cons x y))"},Lemma {lemmaName = "lemma8", lemmaSource = Nothing, hLemmas = ["lemma8"], indVar = Nothing, formula = " (count x (nil :: list P.Int)) === (0)"},Lemma {lemmaName = "lemma7", lemmaSource = Nothing, hLemmas = ["lemma7"], indVar = Nothing, formula = " (cons y (z ++ x2)) === ((cons y z) ++ x2)"},Lemma {lemmaName = "lemma6", lemmaSource = Nothing, hLemmas = ["lemma4"], indVar = Just [0], formula = " ((y ++ z) ++ x2) === (y ++ (z ++ x2))"},Lemma {lemmaName = "lemma5", lemmaSource = Nothing, hLemmas = ["lemma5"], indVar = Nothing, formula = " ((nil :: list x) ++ y) === y"},Lemma {lemmaName = "lemma4", lemmaSource = Nothing, hLemmas = [], indVar = Just [0], formula = " (y ++ (nil :: list x)) === y"},Lemma {lemmaName = "lemma0", lemmaSource = Just "ISort.prop_countcount", hLemmas = [], indVar = Just [0], formula = "\n  ((count x bs) P.+ (count x cs)) === (count x (bs ++ cs))"}])