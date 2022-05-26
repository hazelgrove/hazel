open HTyp;

/* opaque type constructors */
let%test _ = HTypTest.test_read("?", hole());
let%test _ = HTypTest.test_read("Int", int());
let%test _ = HTypTest.test_read("Float", float());
let%test _ = HTypTest.test_read("Bool", bool());
let%test _ = HTypTest.test_read("? -> ?", arrow(hole(), hole()));
let%test _ = HTypTest.test_read("Int -> Bool", arrow(int(), bool()));
let%test _ = HTypTest.test_read("? | ?", sum(hole(), hole()));
let%test _ = HTypTest.test_read("Float | Int", sum(float(), int()));
let%test _ = HTypTest.test_read("[?]", list(hole()));
let%test _ = HTypTest.test_read("[Int]", list(int()));
let%test _ = HTypTest.test_read("? , ?", product([hole(), hole()]));
let%test _ = HTypTest.test_read("Bool , Float", product([bool(), float()]));
let%test _ =
  HTypTest.test_read("[Int] -> Bool", arrow(list(int()), bool()));
