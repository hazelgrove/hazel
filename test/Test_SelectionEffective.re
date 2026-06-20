open Alcotest;
open Haz3lcore;

let effective_segment_string = (input: string): string => {
  let z = Test_Editing.mk_zipper(input);
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let statics =
    CachedStatics.init_from_term(
      ~settings=Test_Editing.default_settings,
      ~is_dynamic_term=true,
      term,
    );
  let syntax =
    CachedSyntax.mk(z, ~info_map=statics.info_map, ~dyn_map=Id.Map.empty);
  SelectionEffective.associative_segment(
    ~info_map=statics.info_map,
    ~term_data=syntax.term_data,
    z,
  )
  |> Printer.of_segment(~holes="?", ~concave_holes="~", ~indent=" ");
};

let test = (~name, ~input, ~expected) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      expected,
      expected,
      effective_segment_string(input),
    )
  );

let tests = (
  "SelectionEffective",
  [
    test(
      ~name="associative selection snaps to left edge of nested product",
      ~input={|2 * 3 + 4 * 5 * 6 §+ "abc"¦|},
      ~expected={|4 * 5 * 6 + "abc"|},
    ),
    test(
      ~name="equality selection snaps over application left operand",
      ~input={|rev(rev(xs)) §== xs¦|},
      ~expected={|rev(rev(xs)) == xs|},
    ),
    test(
      ~name="equality selection snaps over projected left operand",
      ~input={|^^fold(rev(rev(xs))) §== xs¦|},
      ~expected={|^^fold(rev(rev(xs))) == xs|},
    ),
    test(
      ~name=
        "associative selection inside function argument snaps over application",
      ~input={|sin(§x+y¦)|},
      ~expected={|sin(x+y)|},
    ),
  ],
);
