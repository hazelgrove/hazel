open Alcotest;
open Language;

module F = IdTagged.FreshGrammar;

let typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);
let int = F.Typ.int();
let bool = F.Typ.bool();
let x_asc = ty => F.Pat.asc(F.Pat.var("x"), ty);

/* The type the value member [x] of [item] is read at, if it declares one. */
let val_typ = (x, item: Sig.t): option(Typ.t) =>
  switch (Sig.member_of_item(item)) {
  | Some(Val(y, ty)) when String.equal(x, y) => Some(ty)
  | Some(Val(_) | TypeManifest(_))
  | None => None
  };

let member_tests = (
  "Sig.member_of_item",
  [
    test_case("a `let` item's annotation", `Quick, () =>
      check(
        option(typ),
        "x : Int",
        Some(int),
        val_typ("x", F.Sig.sig_let(x_asc(int))),
      )
    ),
    test_case("a `let` item's annotation under a projector", `Quick, () =>
      check(
        option(typ),
        "x : Int",
        Some(int),
        val_typ(
          "x",
          F.Sig.sig_let(
            F.Pat.projector(
              {
                kind: Fold,
                model: "",
              },
              x_asc(int),
            ),
          ),
        ),
      )
    ),
    test_case("a `let` item's annotation under a label", `Quick, () =>
      check(
        option(typ),
        "x : Int",
        Some(int),
        val_typ(
          "x",
          F.Sig.sig_let(F.Pat.tup_label(F.Pat.label("a"), x_asc(int))),
        ),
      )
    ),
    test_case("a `let` item without an annotation is read at ?", `Quick, () =>
      check(
        option(typ),
        "x : ?",
        Some(F.Typ.unknown(Internal)),
        val_typ("x", F.Sig.sig_let(F.Pat.var("x"))),
      )
    ),
  ],
);

let lookup_tests = (
  "Sig.find_value and Sig.find_type_def",
  [
    test_case(
      "the last member of a name",
      `Quick,
      () => {
        let members =
          Sig.members([
            F.Sig.sig_type(F.TPat.var("T"), int),
            F.Sig.sig_let(x_asc(int)),
            F.Sig.sig_type(F.TPat.var("T"), bool),
          ]);
        check(
          option(typ),
          "type member",
          Some(bool),
          Sig.find_type_def(members, "T"),
        );
        check(
          option(typ),
          "value member",
          Some(int),
          Sig.find_value(members, "x"),
        );
        check(
          option(typ),
          "missing",
          None,
          Sig.find_value(members, "nope"),
        );
      },
    ),
  ],
);

let tests = [member_tests, lookup_tests];
