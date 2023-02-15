open Tezt;
open Tezt.Base;

let () =
  Test.register(
    ~__FILE__,
    ~title="example unit test",
    ~tags=["example", "addition"],
    () => {
      if (1 + 1 != 2) {
        Test.fail("1 + 1 is not 2");
      };
      unit;
    },
  );
