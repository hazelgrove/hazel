open Tezt;
open Tezt.Base;

open Hazelctest.Case;

let register_test_with_eval = (title, tags, source) =>
  Test.register(~__FILE__, ~title, ~tags=["hazelc"] @ tags, () =>
    test_with_eval(source) ? unit : Test.fail("evaluation failed!")
  );

let register_test = (title, tags, source, expect) =>
  Test.register(~__FILE__, ~title, ~tags=["hazelc"] @ tags, () =>
    test(source, expect) ? unit : Test.fail("evaluation failed!")
  );
