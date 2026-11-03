/* What binds a type name in the types statics records, so that a type shown
   to the user can point back at where its names were bound. */

open Alcotest;
open Language;
open Test_Statics_Prelude;

/* The (rec binder, alias binder) id pairs the alias [name] has in the
   contexts statics recorded for [code]. */
let rec_binder_ids = (name: string, code: string): list((Id.t, Id.t)) =>
  Id.Map.fold(
    (_, info: Info.t, pairs) => {
      let ctx = Info.ctx_of(info);
      switch (Ctx.lookup_alias(ctx, name), Ctx.lookup_tvar_id(ctx, name)) {
      | (Some({term: Rec(tp, _), _}), Some(id)) => [
          (TPat.rep_id(tp), id),
          ...pairs,
        ]
      | _ => pairs
      };
    },
    statics(parse_exp(code)),
    [],
  );

let tests = (
  "Statics.TypeBinders",
  [
    test_case(
      "a recursive alias's rec type is bound by the alias's own pattern",
      `Quick,
      () => {
        let pairs =
          rec_binder_ids("E", "type E = Int -> E in fun (g : E) -> g");
        check(
          bool,
          "the alias is in some context",
          false,
          List.is_empty(pairs),
        );
        List.iter(
          ((rec_binder, alias)) =>
            check(
              string,
              "the rec binder's id",
              Id.to_string(alias),
              Id.to_string(rec_binder),
            ),
          pairs,
        );
      },
    ),
  ],
);
