("87.8784s",[Lemma {lemmaName = "lemma25", lemmaSource = Nothing, hLemmas = ["Tree.prop_Flatten2"], indVar = Nothing, formula = " (flatten2 x (nil :: list P.Int)) === (flatten0 x)"},Lemma {lemmaName = "lemma2", lemmaSource = Just "Tree.prop_Flatten2", hLemmas = ["Tree.prop_Flatten1","lemma26","lemma5"], indVar = Nothing, formula = " (flatten2 p (nil :: list P.Int)) === (flatten0 p)"},Lemma {lemmaName = "lemma0", lemmaSource = Just "Tree.prop_Flatten1", hLemmas = ["lemma26","lemma27","lemma5"], indVar = Nothing, formula = "\n  (flatten1 (cons p (nil :: list Tree))) === (flatten0 p)"},Lemma {lemmaName = "lemma27", lemmaSource = Nothing, hLemmas = ["lemma27","lemma20"], indVar = Just [0], formula = " (flatten1 (cons x y)) === (flatten2 x (flatten1 y))"},Lemma {lemmaName = "lemma26", lemmaSource = Nothing, hLemmas = ["lemma26","lemma15","lemma7"], indVar = Just [0], formula = " ((flatten0 x) ++ y) === (flatten2 x y)"},Lemma {lemmaName = "lemma23", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (flatten3 (Node Nil x y)) === (cons x (flatten3 y))"},Lemma {lemmaName = "lemma22", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (flatten3 Nil) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma21", lemmaSource = Nothing, hLemmas = ["lemma20"], indVar = Nothing, formula = "\n  (flatten2 x (cons y (flatten2 z x2))) ===\n    (flatten2 (Node x y z) x2)"},Lemma {lemmaName = "lemma20", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (flatten2 (Node Nil x y) z) === (cons x (flatten2 y z))"},Lemma {lemmaName = "lemma19", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = "\n  (flatten2 (Node x y Nil) z) === (flatten2 x (cons y z))"},Lemma {lemmaName = "lemma18", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (flatten2 Nil x) === x"},Lemma {lemmaName = "lemma17", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (flatten1 (cons Nil x)) === (flatten1 x)"},Lemma {lemmaName = "lemma16", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (flatten1 (nil :: list Tree)) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma15", lemmaSource = Nothing, hLemmas = ["lemma5","lemma7"], indVar = Nothing, formula = "\n  ((flatten0 x) ++ (cons y (flatten0 z))) ===\n    (flatten0 (Node x y z))"},Lemma {lemmaName = "lemma14", lemmaSource = Nothing, hLemmas = ["lemma5","lemma7"], indVar = Nothing, formula = "\n  ((flatten0 x) ++ (cons y (nil :: list P.Int))) ===\n    (flatten0 (Node x y Nil))"},Lemma {lemmaName = "lemma13", lemmaSource = Nothing, hLemmas = ["lemma5","lemma7"], indVar = Nothing, formula = " (flatten0 (Node Nil x y)) === (cons x (flatten0 y))"},Lemma {lemmaName = "lemma12", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (flatten0 Nil) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma10", lemmaSource = Nothing, hLemmas = ["lemma5","lemma7"], indVar = Nothing, formula = "\n  (concatMap z (cons x2 (nil :: list y))) === (P.id z x2)"},Lemma {lemmaName = "lemma9", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (concatMap y (nil :: list x)) === (nil :: list x)"},Lemma {lemmaName = "lemma8", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " (cons y (z ++ x2)) === ((cons y z) ++ x2)"},Lemma {lemmaName = "lemma7", lemmaSource = Nothing, hLemmas = ["lemma7"], indVar = Just [0], formula = " ((y ++ z) ++ x2) === (y ++ (z ++ x2))"},Lemma {lemmaName = "lemma6", lemmaSource = Nothing, hLemmas = [], indVar = Nothing, formula = " ((nil :: list x) ++ y) === y"},Lemma {lemmaName = "lemma5", lemmaSource = Nothing, hLemmas = ["lemma5"], indVar = Just [0], formula = " (y ++ (nil :: list x)) === y"}])