("409.774497s",[Lemma {lemmaName = "lemma50", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (insert (0) (cons (count x y) z)) ===\n    (cons (0) (cons (count x y) z))"},Lemma {lemmaName = "lemma45", lemmaSource = Nothing, hLemmas = ["ISort.prop_countcount"], indVar = Nothing, formula = "\n  ((count x y) P.+ (count x z)) === (count x (y ++ z))"},Lemma {lemmaName = "lemma43", lemmaSource = Nothing, hLemmas = ["lemma4","lemma9"], indVar = Nothing, formula = "\n  (ordered (cons x (insert x y))) ===\n    (ordered (cons x (cons x y)))"},Lemma {lemmaName = "lemma32", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (ordered (cons x (nil :: list P.Int))) ===\n    (ordered (nil :: list P.Int))"},Lemma {lemmaName = "lemma30", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (isort (cons x y)) === (insert x (isort y))"},Lemma {lemmaName = "lemma28", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (isort (nil :: list P.Int)) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma25", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (insert x (nil :: list P.Int)) === (cons x (nil :: list P.Int))"},Lemma {lemmaName = "lemma21", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (count (0) (cons (x P.+ x) y)) === (count (0) (cons x y))"},Lemma {lemmaName = "lemma15", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (count y (cons x (nil :: list P.Int))) ===\n    (count x (cons y (nil :: list P.Int)))"},Lemma {lemmaName = "lemma13", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (count (1) (cons (0) x)) === (count (1) x)"},Lemma {lemmaName = "lemma12", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (count (0) (cons (1) x)) === (count (0) x)"},Lemma {lemmaName = "lemma10", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " ((0) P.<= (count x y)) === P.True"},Lemma {lemmaName = "lemma9", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " ((1) P.+ (count x y)) === (count x (cons x y))"},Lemma {lemmaName = "lemma8", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (count x (nil :: list P.Int)) === (0)"},Lemma {lemmaName = "lemma7", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (cons y (z ++ x2)) === ((cons y z) ++ x2)"},Lemma {lemmaName = "lemma6", lemmaSource = Nothing, hLemmas = [], indVar = Just [0], formula = " ((y ++ z) ++ x2) === (y ++ (z ++ x2))"},Lemma {lemmaName = "lemma5", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " ((nil :: list x) ++ y) === y"},Lemma {lemmaName = "lemma4", lemmaSource = Nothing, hLemmas = [], indVar = Just [0], formula = " (y ++ (nil :: list x)) === y"},Lemma {lemmaName = "lemma0", lemmaSource = Just "ISort.prop_countcount", hLemmas = [], indVar = Just [0], formula = "\n  ((count x bs) P.+ (count x cs)) === (count x (bs ++ cs))"}])