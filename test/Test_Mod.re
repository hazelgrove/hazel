open Alcotest;
open Language;

module F = IdTagged.FreshGrammar;

/* Module items as dynamics sees them: a pending binding of either form
   shadows an evaluated one for the items after it. */
let member_tests = {
  let evaluated_m = Mod.fresh(ModVal("M", F.Exp.var("v0")));
  let stuck = F.Mod.mod_let(F.Pat.var("a"), F.Exp.empty_hole());
  let member_name = (items, name) =>
    switch (Mod.member(items, name)) {
    | Some({term: Var(x), _}) => Some(x)
    | Some(_)
    | None => None
    };
  (
    "Mod.member",
    [
      test_case(
        "an evaluated binding before the stuck item is a member", `Quick, () =>
        check(
          option(string),
          "M",
          Some("v0"),
          member_name([evaluated_m, stuck], "M"),
        )
      ),
      test_case(
        "a pending let rebinding the name leaves the member stuck", `Quick, () =>
        check(
          option(string),
          "stuck",
          None,
          member_name(
            [
              evaluated_m,
              stuck,
              F.Mod.mod_let(F.Pat.var("M"), F.Exp.var("v1")),
            ],
            "M",
          ),
        )
      ),
      test_case(
        "a pending module item rebinding the name leaves the member stuck",
        `Quick,
        () =>
        check(
          option(string),
          "stuck",
          None,
          member_name(
            [
              evaluated_m,
              stuck,
              F.Mod.module_mod(F.MPat.var("M"), F.Exp.var("v1")),
            ],
            "M",
          ),
        )
      ),
    ],
  );
};

let bound_names_tests = (
  "Mod.bound_names",
  [
    test_case("the function-definition sugar binds its name only", `Quick, () =>
      check(
        list(string),
        "f",
        ["f"],
        Mod.bound_names(
          F.Mod.mod_let(
            F.Pat.ap(F.Pat.var("f"), F.Pat.var("x")),
            F.Exp.var("x"),
          ),
        ),
      )
    ),
    test_case("a module item binds its name", `Quick, () =>
      check(
        list(string),
        "N",
        ["N"],
        Mod.bound_names(
          F.Mod.module_mod(F.MPat.var("N"), F.Exp.module_([])),
        ),
      )
    ),
  ],
);

let parts_tests = {
  let e = F.Exp.var("v");
  let ty = F.Typ.int();
  let p = F.Pat.var("x");
  let mp = F.MPat.var("M");
  let tp = F.TPat.var("T");
  let id_of = part => Option.map(Any.rep_id, part) |> Option.map(Id.show);
  let check_parts = (name, item, ~pat, ~def) => {
    check(option(string), name ++ ": pat", pat, id_of(Mod.pat(item)));
    check(option(string), name ++ ": def", def, id_of(Mod.def(item)));
  };
  (
    "Mod.pat and Mod.def",
    [
      test_case(
        "the written parts of each item kind",
        `Quick,
        () => {
          let e_id = Some(Id.show(Exp.rep_id(e)));
          let ty_id = Some(Id.show(Typ.rep_id(ty)));
          check_parts(
            "let",
            F.Mod.mod_let(p, e),
            ~pat=Some(Id.show(Pat.rep_id(p))),
            ~def=e_id,
          );
          check_parts(
            "module",
            F.Mod.module_mod(mp, e),
            ~pat=Some(Id.show(MPat.rep_id(mp))),
            ~def=e_id,
          );
          check_parts(
            "type",
            F.Mod.mod_type(tp, ty),
            ~pat=Some(Id.show(TPat.rep_id(tp))),
            ~def=ty_id,
          );
          check_parts("expression", F.Mod.mod_exp(e), ~pat=None, ~def=e_id);
          check_parts(
            "evaluated binding",
            Mod.fresh(ModVal("x", e)),
            ~pat=None,
            ~def=e_id,
          );
          check_parts("hole", Mod.fresh(EmptyHole), ~pat=None, ~def=None);
        },
      ),
    ],
  );
};

let of_exp_tests = {
  let id = testable(Fmt.using(Id.show, Fmt.string), Id.equal);
  (
    "Mod.of_exp",
    [
      test_case(
        "a menhir-parsed bare expression item is its expression",
        `Quick,
        () => {
          let (exp, _) =
            MenhirParser.Conversion.Exp.get_indicated_ids(
              MenhirParser.Conversion.Exp.of_menhir_ast(
                MenhirParser.Interface.parse_program("{ let x = 1; x + 1 }"),
              ),
            );
          let item_and_exp_ids =
            switch (exp.term) {
            | Module(items) =>
              List.filter_map(
                (item: Mod.t) =>
                  switch (item.term) {
                  | ModExp(e) => Some((Mod.rep_id(item), Exp.rep_id(e)))
                  | _ => None
                  },
                items,
              )
            | _ => []
            };
          check(
            bool,
            "there is a bare expression item",
            false,
            List.is_empty(item_and_exp_ids),
          );
          check(
            list(pair(id, id)),
            "shared ids",
            List.map(((i, _)) => (i, i), item_and_exp_ids),
            item_and_exp_ids,
          );
        },
      ),
    ],
  );
};

let tests = [member_tests, bound_names_tests, parts_tests, of_exp_tests];
