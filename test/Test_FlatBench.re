open Alcotest;
open Haz3lcore;
open Language;

/* depth-scaling probe: flat let-chains where every item references
   the FIRST binding (maximum lookup depth). If cold calc scales
   superlinearly in n, list-ctx lookup depth matters at this shape. */
let flat_chain = (n: int): string =>
  String.concat(
    "",
    ["let x1 = 1 in\n"]
    @ List.init(n - 1, i =>
        Printf.sprintf("let x%d = x1 + x%d in\n", i + 2, i + 1)
      )
    @ ["x1"],
  );

let case = () => {
  List.iter(
    n => {
      let src = flat_chain(n);
      switch (
        FastParse.of_text(
          ~materialize=Triggers.invoked_projector,
          ~collect_refractors=true,
          ~root=Exp,
          src,
        )
      ) {
      | None => fail("unparseable")
      | Some(seg) =>
        let term = MakeTerm.go(seg).term;
        let t0 = Sys.time();
        let ds = DefStatics.calc(~settings=CoreSettings.on, term);
        Printf.printf(
          "FLAT n=%4d: cold calc %.0f ms (%d items)\n",
          n,
          (Sys.time() -. t0) *. 1000.0,
          List.length(ds.items),
        );
      };
    },
    [500, 1000, 2000, 4000],
  );
  check(bool, "ran", true, true);
};

let tests = ("FlatBench", [test_case("depth scaling", `Quick, case)]);
