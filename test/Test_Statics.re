open Alcotest;
open Haz3lcore;

// /*Create a testable type for dhexp which requires
//   an equal function (dhexp_eq) and a print function (dhexp_print) */
let statics_map =
  testable(
    Fmt.using([%derive.show: UExp.t(Typ.t(unit))], Fmt.string),
    (==),
  );

let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);

let typ = (typ: Typ.term(unit)): Typ.t(unit) => {
  term: typ,
  annotation: (),
};

let get_statics = exp =>
  Statics.uexp_to_info_map(~ctx=[], ~ancestors=[], exp, Id.Map.empty) |> snd;

let sample_int: Exp.t(IdTag.t) = {
  term: Int(9),
  annotation: {
    ids: [id_at(0)],
    copied: false,
  },
};
// Take the statics map and the expression tagged with id and interleave statics throughout the annotations
let run_statics = exp => {
  let static_map = get_statics(exp);
  let traverse = (exp: UExp.t(IdTag.t)): Exp.t(Typ.t(unit)) => {
    let baz: Exp.t(Typ.t(unit)) =
      Exp.map_annotation(
        (idtag: IdTag.t) => {
          let bar = List.hd(idtag.ids);
          let foo = Id.Map.find(bar, static_map);
          switch (foo) {
          | InfoExp(e) => Typ.map_annotation((_: IdTag.t) => (), e.ty)
          | _ => raise(Util.NotYetImplemented("Extract type from info"))
          };
        },
        exp,
      );
    baz;
  };
  traverse(exp);
};

let single_integer = () => {
  let expected: UExp.t(Typ.t(unit)) = {
    term: Int(9),
    annotation: {
      term: Int,
      annotation: (),
    },
  };
  let actual =
    run_statics({
      term: Int(9),
      annotation: {
        ids: [id_at(0)],
        copied: false,
      },
    });
  Alcotest.check(statics_map, "foo", expected, actual);
};
let boolean = () => {
  let expected: UExp.t(Typ.t(unit)) = {
    term: Bool(false),
    annotation: {
      term: Bool,
      annotation: (),
    },
  };
  let actual =
    run_statics({
      term: Bool(false),
      annotation: {
        ids: [id_at(0)],
        copied: false,
      },
    });
  Alcotest.check(statics_map, "ascribed with boolean", expected, actual);
};

let tests = (
  "Statics",
  [
    test_case("Single integer", `Quick, single_integer),
    test_case("Boolean", `Quick, boolean),
  ],
);
