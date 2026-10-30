open Test_Statics_Prelude;
open Alcotest;
open Language;
open TypExpectation;

/* Direct-API helper for exercising `utyp_to_info_map` against a given
   `TypExpectation.t`. Relocated from the old test/Test_Info.re; these tests
   are still the most direct way to check per-typ-position status without
   threading the specific expectation through concrete syntax. */
module DeriveTypStatus = {
  let show_typ_status =
      ((marks, ok): (list(Mark.t), option(Message.ok_typ))): string =>
    "{"
    ++ [%derive.show: list(Mark.t)](marks)
    ++ ", "
    ++ [%derive.show: option(Message.ok_typ)](ok)
    ++ "}";

  let equal_typ_status =
      (
        (m1, o1): (list(Mark.t), option(Message.ok_typ)),
        (m2, o2): (list(Mark.t), option(Message.ok_typ)),
      )
      : bool =>
    m1 == m2 && o1 == o2;

  let derive = (ctx: Ctx.t, expects: TypExpectation.t, ty: Typ.t) => {
    let (_, m) =
      Statics.utyp_to_info_map(
        ~ctx,
        ~expects,
        ~ancestors=[],
        ty,
        Statics.Map.empty,
      );
    switch (Statics.Map.lookup_typ(Typ.rep_id(ty), m)) {
    | Some(info) =>
      let ok =
        switch (info.message) {
        | Some(Message.TypOk(ok)) => Some(ok)
        | _ => None
        };
      (info.marks, ok);
    | None => ([], None)
    };
  };
};

let typ_status_testable =
  testable(
    Fmt.using(DeriveTypStatus.show_typ_status, Fmt.string),
    DeriveTypStatus.equal_typ_status,
  );

let derive_typ_tests = [
  test_case(
    "Typ meet on polymorphic types",
    `Quick,
    () => {
      let status =
        DeriveTypStatus.derive(
          Builtins.ctx_init(Some(Int)),
          TypeExpected,
          Unknown(Hole(Invalid("x"))) |> Typ.temp,
        );

      check(
        typ_status_testable,
        "Bad token",
        ([Mark.BadToken("x")], None),
        status,
      );
    },
  ),
  /* Unknown(EmptyHole) */
  test_case(
    "EmptyHole under LabelExpected => EmptyLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Unknown(Hole(EmptyHole)) |> Typ.temp;
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Unique, []), ty);
      check(
        typ_status_testable,
        "Empty label",
        ([], Some(Message.EmptyLabel)),
        status,
      );
    },
  ),
  test_case(
    "EmptyHole under TypeExpected => Type(ty)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Unknown(Hole(EmptyHole)) |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Type of EmptyHole",
        ([], Some(Message.Type(ty))),
        status,
      );
    },
  ),
  /* Unknown(MultiHole) */
  test_case(
    "MultiHole => ParseFailure",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Unknown(Hole(MultiHole([]))) |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Parse failure",
        ([Mark.TypParseFailure], None),
        status,
      );
    },
  ),
  /* Var(name) with TypeExpected cases */
  test_case(
    "Var alias under TypeExpected => TypeAlias",
    `Quick,
    () => {
      let ctx0 = Builtins.ctx_init(Some(Int));
      let ctx =
        Ctx.extend_alias(ctx0, "A", Id.invalid, Atom(Int) |> Typ.temp);
      let ty = Var("A") |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Alias is a type",
        (
          [],
          Some(Message.TypeAlias("A", Typ.weak_head_normalize(ctx, ty))),
        ),
        status,
      );
    },
  ),
  test_case(
    "Free type var under TypeExpected => FreeTypeVariable",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Var("X") |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Free type variable",
        ([Mark.TypFreeTypeVariable("X")], None),
        status,
      );
    },
  ),
  test_case(
    "Abstract tvar under TypeExpected => Type(Var)",
    `Quick,
    () => {
      let ctx0 = Builtins.ctx_init(Some(Int));
      let ctx =
        Ctx.extend_tvar(
          ctx0,
          {
            name: "T",
            id: Id.invalid,
            kind: Abstract,
          },
        );
      let ty = Var("T") |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Abstract type variable",
        ([], Some(Message.Type(Var("T") |> Typ.temp))),
        status,
      );
    },
  ),
  test_case(
    "Var non-label under LabelExpected => WantLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Var("A") |> Typ.temp; /* not an alias in this ctx */
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Unique, []), ty);
      check(
        typ_status_testable,
        "WantLabel",
        ([Mark.TypWantLabel], None),
        status,
      );
    },
  ),
  /* Var(name) with Constructor/VariantExpected */
  test_case(
    "Var under ConstructorExpected Unique => Variant(name, sum)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let sum_ty = Option.get(Ctx.lookup_alias(ctx, "Option"));
      let ty = Var("Some") |> Typ.temp;
      check(
        typ_status_testable,
        "Variant unique",
        ([], Some(Message.Variant("Some", sum_ty))),
        DeriveTypStatus.derive(ctx, ConstructorExpected(Unique, sum_ty), ty),
      );
      check(
        typ_status_testable,
        "Variant unique",
        ([], Some(Message.Variant("Some", sum_ty))),
        DeriveTypStatus.derive(ctx, VariantExpected(Unique, sum_ty), ty),
      );
    },
  ),
  test_case(
    "Var under ConstructorExpected Duplicate => DuplicateConstructor",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let sum_ty = Option.get(Ctx.lookup_alias(ctx, "Option"));
      let ty = Var("Some") |> Typ.temp;
      check(
        typ_status_testable,
        "Duplicate constructor",
        ([Mark.TypDuplicateConstructor("Some")], None),
        DeriveTypStatus.derive(
          ctx,
          ConstructorExpected(Duplicate, sum_ty),
          ty,
        ),
      );
      check(
        typ_status_testable,
        "Duplicate constructor variant",
        ([Mark.TypDuplicateConstructor("Some")], None),
        DeriveTypStatus.derive(ctx, VariantExpected(Duplicate, sum_ty), ty),
      );
    },
  ),
  /* Label(name) term cases */
  test_case(
    "Label under TypeExpected => Type(ty)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Label as type",
        ([], Some(Message.Type(ty))),
        status,
      );
    },
  ),
  test_case(
    "Label under LabelExpected Unique => Type(ty)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Unique, []), ty);
      check(
        typ_status_testable,
        "Label ok",
        ([], Some(Message.Type(ty))),
        status,
      );
    },
  ),
  test_case(
    "Label under LabelExpected Duplicate (present) => Duplicate",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let dupes = ["a", "b"];
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Duplicate, dupes), ty);
      check(
        typ_status_testable,
        "Duplicate label",
        ([Mark.DuplicateLabel("a", ty)], None),
        status,
      );
    },
  ),
  test_case(
    "Label under LabelExpected Duplicate (absent) => WantLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Duplicate, ["x"]), ty);
      check(
        typ_status_testable,
        "WantLabel when not expected duplicate",
        ([Mark.TypWantLabel], None),
        status,
      );
    },
  ),
  test_case(
    "Label under LabelProjectionExpected (included) => Type(ty)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("p") |> Typ.temp;
      let status =
        DeriveTypStatus.derive(
          ctx,
          LabelProjectionExpected(Some(["p", "q"])),
          ty,
        );
      check(
        typ_status_testable,
        "Projection ok",
        ([], Some(Message.Type(ty))),
        status,
      );
    },
  ),
  test_case(
    "Label under LabelProjectionExpected (not included) => InvalidLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("p") |> Typ.temp;
      let labels = ["x", "y"];
      let status =
        DeriveTypStatus.derive(
          ctx,
          LabelProjectionExpected(Some(labels)),
          ty,
        );
      check(
        typ_status_testable,
        "Invalid projection label",
        ([Mark.InvalidLabel("p", labels)], None),
        status,
      );
    },
  ),
  test_case(
    "Label under ConstructorExpected => WantConstructorFoundType",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let sum_ty = Option.get(Ctx.lookup_alias(ctx, "Option"));
      let status =
        DeriveTypStatus.derive(ctx, ConstructorExpected(Unique, sum_ty), ty);
      check(
        typ_status_testable,
        "Label where constructor expected",
        ([Mark.TypWantConstructorFoundType(ty)], None),
        status,
      );
    },
  ),
  test_case(
    "Label under VariantExpected => WantConstructorFoundType",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Label("a") |> Typ.temp;
      let sum_ty = Option.get(Ctx.lookup_alias(ctx, "Option"));
      let status =
        DeriveTypStatus.derive(ctx, VariantExpected(Unique, sum_ty), ty);
      check(
        typ_status_testable,
        "Label where variant expected",
        ([Mark.TypWantConstructorFoundType(ty)], None),
        status,
      );
    },
  ),
  /* Default branch on other types */
  test_case(
    "Other type under TypeExpected => Type(ty)",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Atom(Int) |> Typ.temp;
      let status = DeriveTypStatus.derive(ctx, TypeExpected, ty);
      check(
        typ_status_testable,
        "Atom type ok",
        ([], Some(Message.Type(ty))),
        status,
      );
    },
  ),
  test_case(
    "Other type under LabelExpected => WantLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Atom(Int) |> Typ.temp;
      let status =
        DeriveTypStatus.derive(ctx, LabelExpected(Unique, []), ty);
      check(
        typ_status_testable,
        "WantLabel for non-label",
        ([Mark.TypWantLabel], None),
        status,
      );
    },
  ),
  test_case(
    "Other type under LabelProjectionExpected => WantLabel",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Atom(Int) |> Typ.temp;
      let status =
        DeriveTypStatus.derive(
          ctx,
          LabelProjectionExpected(Some(["a"])),
          ty,
        );
      check(
        typ_status_testable,
        "WantLabel for non-label (projection)",
        ([Mark.TypWantLabel], None),
        status,
      );
    },
  ),
  test_case(
    "Other type under ConstructorExpected => WantConstructorFoundType",
    `Quick,
    () => {
      let ctx = Builtins.ctx_init(Some(Int));
      let ty = Atom(Int) |> Typ.temp;
      let sum_ty = Option.get(Ctx.lookup_alias(ctx, "Option"));
      let status =
        DeriveTypStatus.derive(ctx, ConstructorExpected(Unique, sum_ty), ty);
      check(
        typ_status_testable,
        "Constructor expected elsewhere",
        ([Mark.TypWantConstructorFoundType(ty)], None),
        status,
      );
    },
  ),
];

/* [source] has exactly the warnings [defined_as_themselves], and
   [error_count] errors. */
let defined_as_itself =
    (
      name,
      source,
      defined_as_themselves: list(Warning.list_item),
      error_count,
    ) =>
  test_case(
    name,
    `Quick,
    () => {
      let map = statics(parse_exp(source));
      let warned =
        Id.Map.fold(
          (id, info, acc) =>
            Id.equal(id, Info.id_of(info))
              ? Info.warnings_of(info) @ acc : acc,
          map,
          [],
        );
      let shown = ws =>
        List.sort(String.compare, List.map(Warning.show_list_item, ws));
      check(
        list(string),
        "types defined as themselves",
        shown(defined_as_themselves),
        shown(warned),
      );
      check(
        int,
        "errors",
        error_count,
        List.length(Statics.Map.error_ids(map)),
      );
    },
  );

let tests = (
  "Statics.Types",
  [
    fully_consistent_typecheck(
      "Type alias works for typfun variable",
      {|typfun a -> fun y ->
  let x :a =  ? in
  type F = a in
  x : F|},
      Some(
        FTemp.(
          Typ.(poly(TPat.var("a"), arrow(unknown(Internal), var("a"))))
        ),
      ),
    ),
    /* A later alias of the same name must not capture references made by
       earlier aliases: `Skel` means the first `T` (as in OCaml, cf.
       haz3lcore's Skel.Stacks), so resolving the second `T` terminates. */
    fully_consistent_typecheck(
      "shadowing alias does not capture an earlier alias's reference",
      {|type T = + A + B(T) in
type Skel = T in
type T = (output=[Skel]) in
let x : T = (output=[B(A)]) in
1|},
      Some(FTemp.Typ.int()),
    ),
    fully_consistent_typecheck(
      "the same shadowing inside a module (haz3lcore's Skel.Stacks)",
      {|type T = + A + B(T) in
module Stacks = {
  type Skel = T;
  type T = (output=[Skel]);
  let x : T = (output=[B(A)])
} in
let y : Stacks.T = (output=[A]) in
1|},
      Some(FTemp.Typ.int()),
    ),
    fully_consistent_typecheck(
      ~normalize=true,
      "shadowed alias resolves to the binding in scope at its definition",
      {|type S = Int in
type T = (a=S) in
type S = Bool in
let x : T = (a=1) in
x.a + 1|},
      Some(FTemp.Typ.int()),
    ),
    fully_consistent_typecheck(
      "alias cycle through shadowing terminates",
      {|type A = Int in
type B = A in
type A = B in
let x : A = 1 in
let y : B = x in
y + 1|},
      Some(FTemp.Typ.int()),
    ),
    fully_consistent_typecheck(
      "alias shadowing a type parameter terminates",
      {|typfun T -> fun (t : T) ->
  type B = T in
  type T = B in
  let u : T = t in
  u|},
      Some(FTemp.(Typ.(poly(TPat.var("T"), arrow(var("T"), var("T")))))),
    ),
    synthesizes(
      "alias captured by a later binding of a name it used while unbound",
      {|type B = A in
type A = B in
let x : A = 1 in
1|},
      Some(FTemp.Typ.int()),
    ),
    defined_as_itself(
      "A self-referential alias is warned and does not loop (#1621)",
      "type y = y in type ? = y in ?",
      [TPat(NonContractive("y"))],
      0,
    ),
    defined_as_itself(
      "No value has a type defined as itself",
      "type T = T in let a : T = 1 in let b : T = [1] in let c : T = fun x -> x in (a, b, c)",
      [TPat(NonContractive("T"))],
      3,
    ),
    defined_as_itself(
      "A variable or hole of a type defined as itself is that type",
      "type T = T in fun (x : T) -> ((x : T), (? : T))",
      [TPat(NonContractive("T"))],
      0,
    ),
    test_case(
      // https://github.com/hazelgrove/hazel/issues/1624

        "Coverage.all_ctrs_of_typ no infinite recursion on nested non-productive Rec",
        `Quick,
        () => {
          let rec_type =
            FTemp.Typ.(
              rec_(
                FTemp.TPat.var("x"),
                rec_(FTemp.TPat.var("y"), var("x")),
              )
            );
          let mismatch =
            Marks([
              ExpectationMismatch({
                ana: rec_type,
                syn: FTemp.Typ.prod([]),
              }),
            ]);
          annotated_tree_test(
            "fun (() : rec x -> rec y -> x) -> []",
            FTemp.Typ.(arrow(rec_type, list(unknown(Internal)))),
            FIError.Exp.(
              fn(
                FIError.Pat.(
                  asc(
                    tuple(~ann=Some(mismatch), []),
                    FIError.Typ.(
                      rec_(
                        FIError.TPat.var("x"),
                        rec_(FIError.TPat.var("y"), var("x")),
                      )
                    ),
                  )
                ),
                list_lit([]),
                None,
                None,
              )
            ),
          );
        },
      ),
    test_case(
      // https://github.com/hazelgrove/hazel/issues/2235

        "Coverage.all_ctrs_of_typ no infinite recursion on Rec with hole binder",
        `Quick,
        () => {
          let rec_type =
            FTemp.Typ.(
              rec_(
                FTemp.TPat.var("x"),
                rec_(FTemp.TPat.empty_hole(), var("x")),
              )
            );
          let mismatch =
            Marks([
              ExpectationMismatch({
                ana: rec_type,
                syn: FTemp.Typ.prod([]),
              }),
            ]);
          annotated_tree_test(
            "fun (() : rec x -> rec ? -> x) -> 132032.832758",
            FTemp.Typ.(arrow(rec_type, float())),
            FIError.Exp.(
              fn(
                FIError.Pat.(
                  asc(
                    tuple(~ann=Some(mismatch), []),
                    FIError.Typ.(
                      rec_(
                        FIError.TPat.var("x"),
                        rec_(FIError.TPat.empty_hole(), var("x")),
                      )
                    ),
                  )
                ),
                float(132032.832758),
                None,
                None,
              )
            ),
          );
        },
      ),
    skip_known_bug(
      "all_ctrs_of_type called with a non-normalized type", // https://github.com/hazelgrove/hazel/issues/1626
      {|fun (?: (Float((+ A(Bool))))) -> ""|},
    ),
    // These overflowed Typ.normalize and raised Not_found respectively, so
    // reaching a type at all is the regression being pinned.
    synthesizes(
      "self-referential type alias in a pattern annotation (#1623)",
      {|type y = y -> ? in fun (A: (y)) -> a|},
      Some(
        FTemp.Typ.(
          arrow(
            rec_(
              FTemp.TPat.var("y"),
              arrow(var("y"), unknown(Hole(EmptyHole))),
            ),
            unknown(Internal),
          )
        ),
      ),
    ),
    synthesizes(
      "nested rec types over a sum hole (#1999)",
      {|let (B: ((rec ? -> (rec ? -> (+ ?))))) = A in a|},
      Some(FTemp.Typ.unknown(Internal)),
    ),
    inconsistent_typecheck(
      "A recursive occurrence under a differently named binder is not a hole",
      parse_exp("fun (f : rec a -> (a, Int)) -> (f : rec b -> (Bool, Int))"),
    ),
    fully_consistent_typecheck(
      "A tuple checks against a recursive alias and destructures through its unrolling",
      "type T = (T, Int) in let x : T = (?, 3) in let (a, b) = x in b",
      Some(FTemp.Typ.int()),
    ),
    fully_consistent_typecheck(
      "A function checks against a recursive arrow alias, recursively, and applies through it",
      "type T = Int -> T in let f : T = fun x -> f(x) in f(1)",
      Some(FTemp.(Typ.(rec_(TPat.var("T"), arrow(int(), var("T")))))),
    ),
    fully_consistent_typecheck(
      "A labeled tuple is rearranged against a recursive alias through its unrolling",
      "type T = (next=T, val=Int) in let x : T = (val=3, next=?) in let (next=n, val=v) = x in v",
      Some(FTemp.Typ.int()),
    ),
    fully_consistent_typecheck(
      "A recursive alias checks against its other phase",
      "type A = Int -> Int -> A in let f : A = fun x -> fun y -> f in let g : Int -> A = f in 0",
      Some(FTemp.Typ.int()),
    ),
    defined_as_itself(
      "A written self-loop is warned where it is written and meets no other type",
      "fun (y : rec x -> x) -> (y : Int)",
      [Typ(NonContractive("x"))],
      1,
    ),
    defined_as_itself(
      "A self-loop in parentheses is warned and does not loop",
      "fun (y : rec x -> (x)) -> (y : Int)",
      [Typ(NonContractive("x"))],
      1,
    ),
    defined_as_itself(
      "A self-loop under a binder is warned for the binder it loops to",
      "fun (f : rec x -> rec y -> x) -> f",
      [Typ(NonContractive("x"))],
      0,
    ),
    defined_as_itself(
      "A self-loop under a vacuous binder is warned only for the inner binder",
      "fun (f : rec x -> rec y -> y) -> f",
      [Typ(NonContractive("y"))],
      0,
    ),
    inconsistent_typecheck(
      "An argument is checked against the parameter type of a recursive arrow alias",
      parse_exp("type T = Int -> T in fun (f : T) -> f(true)"),
    ),
    fully_consistent_typecheck(
      "A constructor argument checks against a recursive alias through its unrolling",
      "type T = (T, Int) in type L = + C(T) in let v : L = C(?, 2) in 0",
      Some(FTemp.Typ.int()),
    ),
    test_case(
      "Type parse failure",
      `Quick,
      () => {
        // This was https://github.com/hazelgrove/hazel/issues/1459 which used to crash statics
        let exp = parse_exp("type x = Int(Float) in let y : x =  1");
        let s = statics(exp);

        let errors = errors(s) |> List.map(((_, ms)) => Marks(ms));

        check(
          list(testable_issue),
          "Has parse failure error",
          [Marks([Mark.TypParseFailure])],
          errors,
        );
      },
    ),
  ]
  @ derive_typ_tests,
);
