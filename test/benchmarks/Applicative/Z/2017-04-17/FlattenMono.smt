("45.132784s",[Lemma {lemmaName = "lemma28", lemmaSource = Nothing, hLemmas = ["lemma28","Tree.prop_Flatten2"], indVar = Nothing, formula = " (flatten2 x (nil :: list P.Int)) === (flatten0 x)"},Lemma {lemmaName = "lemma4", lemmaSource = Just "Tree.prop_Flatten3", hLemmas = ["lemma4","lemma27"], indVar = Nothing, formula = " (flatten3 p) === (flatten0 p)"},Lemma {lemmaName = "lemma2", lemmaSource = Just "Tree.prop_Flatten2", hLemmas = ["lemma2","lemma29","lemma5"], indVar = Nothing, formula = " (flatten2 p (nil :: list P.Int)) === (flatten0 p)"},Lemma {lemmaName = "lemma1", lemmaSource = Just "Tree.prop_Flatten1List", hLemmas = ["lemma1","lemma26"], indVar = Nothing, formula = " (flatten1 ps) === (concatMapF0 ps)"},Lemma {lemmaName = "lemma29", lemmaSource = Nothing, hLemmas = ["lemma13","lemma7","lemma9","lemma23"], indVar = Just [1], formula = " ((flatten0 x) ++ y) === (flatten2 x y)"},Lemma {lemmaName = "lemma27", lemmaSource = Nothing, hLemmas = ["lemma7"], indVar = Just [0], formula = " (flatten3 x) === (flatten0 x)"},Lemma {lemmaName = "lemma26", lemmaSource = Nothing, hLemmas = ["lemma7","lemma13"], indVar = Just [0], formula = " (flatten1 x) === (concatMapF0 x)"},Lemma {lemmaName = "lemma25", lemmaSource = Nothing, hLemmas = ["lemma25"], indVar = Nothing, formula = " (flatten3 (Node Nil x y)) === (cons x (flatten3 y))"},Lemma {lemmaName = "lemma24", lemmaSource = Nothing, hLemmas = ["lemma24"], indVar = Nothing, formula = " (flatten3 Nil) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma23", lemmaSource = Nothing, hLemmas = ["lemma23"], indVar = Nothing, formula = "\n  (flatten2 x (cons y (flatten2 z x2))) ===\n    (flatten2 (Node x y z) x2)"},Lemma {lemmaName = "lemma22", lemmaSource = Nothing, hLemmas = ["lemma22"], indVar = Nothing, formula = "\n  (flatten2 (Node Nil x y) z) === (cons x (flatten2 y z))"},Lemma {lemmaName = "lemma21", lemmaSource = Nothing, hLemmas = ["lemma21"], indVar = Nothing, formula = "\n  (flatten2 (Node x y Nil) z) === (flatten2 x (cons y z))"},Lemma {lemmaName = "lemma20", lemmaSource = Nothing, hLemmas = ["lemma20"], indVar = Nothing, formula = " (flatten2 Nil x) === x"},Lemma {lemmaName = "lemma19", lemmaSource = Nothing, hLemmas = ["lemma19"], indVar = Nothing, formula = " (flatten1 (cons Nil x)) === (flatten1 x)"},Lemma {lemmaName = "lemma18", lemmaSource = Nothing, hLemmas = ["lemma18"], indVar = Nothing, formula = " (flatten1 (nil :: list Tree)) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma17", lemmaSource = Nothing, hLemmas = ["lemma17"], indVar = Nothing, formula = "\n  ((flatten0 x) ++ (concatMapF0 y)) === (concatMapF0 (cons x y))"},Lemma {lemmaName = "lemma16", lemmaSource = Nothing, hLemmas = ["lemma16"], indVar = Nothing, formula = " (concatMapF0 (cons Nil x)) === (concatMapF0 x)"},Lemma {lemmaName = "lemma15", lemmaSource = Nothing, hLemmas = ["lemma15","lemma5"], indVar = Nothing, formula = "\n  (concatMapF0 (cons x (nil :: list Tree))) === (flatten0 x)"},Lemma {lemmaName = "lemma14", lemmaSource = Nothing, hLemmas = ["lemma14"], indVar = Nothing, formula = " (concatMapF0 (nil :: list Tree)) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma13", lemmaSource = Nothing, hLemmas = ["lemma13","lemma9","lemma7"], indVar = Nothing, formula = "\n  ((flatten0 x) ++ (cons y (flatten0 z))) ===\n    (flatten0 (Node x y z))"},Lemma {lemmaName = "lemma12", lemmaSource = Nothing, hLemmas = ["lemma12","lemma5"], indVar = Nothing, formula = "\n  ((flatten0 x) ++ (cons y (nil :: list P.Int))) ===\n    (flatten0 (Node x y Nil))"},Lemma {lemmaName = "lemma11", lemmaSource = Nothing, hLemmas = ["lemma11"], indVar = Nothing, formula = " (flatten0 (Node Nil x y)) === (cons x (flatten0 y))"},Lemma {lemmaName = "lemma10", lemmaSource = Nothing, hLemmas = ["lemma10"], indVar = Nothing, formula = " (flatten0 Nil) === (nil :: list P.Int)"},Lemma {lemmaName = "lemma9", lemmaSource = Nothing, hLemmas = ["lemma9"], indVar = Nothing, formula = " ((cons x (nil :: list P.Int)) ++ y) === (cons x y)"},Lemma {lemmaName = "lemma8", lemmaSource = Nothing, hLemmas = ["lemma8"], indVar = Nothing, formula = " (cons x (y ++ z)) === ((cons x y) ++ z)"},Lemma {lemmaName = "lemma7", lemmaSource = Nothing, hLemmas = ["lemma5"], indVar = Just [0], formula = " ((x ++ y) ++ z) === (x ++ (y ++ z))"},Lemma {lemmaName = "lemma6", lemmaSource = Nothing, hLemmas = ["lemma6"], indVar = Nothing, formula = " ((nil :: list P.Int) ++ x) === x"},Lemma {lemmaName = "lemma5", lemmaSource = Nothing, hLemmas = [], indVar = Just [0], formula = " (x ++ (nil :: list P.Int)) === x"}])