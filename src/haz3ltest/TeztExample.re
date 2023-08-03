open Tezt;
open Tezt.Base;

Test.register(
  ~__FILE__,
  ~title="Example unit test",
  ~tags=["example", "addition"],
  () => {
    if (1 + 1 != 2) {
      Test.fail("1 + 1 is not 2");
    };
    unit;
  },
);
