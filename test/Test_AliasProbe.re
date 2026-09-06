open Alcotest;
open Haz3lcore;
open Language;

/* CHARACTERIZATION: type-alias references in variable types resolve
   LAZILY against the ctx at the normalization (use) site, so a
   shadowing alias between a binder and a use wins over the alias in
   scope at the binder:
     type T = Int in let x : T = 1 in type T = Bool in x
   reports x : Bool at the use (raw type is the unresolved Var "T").
   Verified empirically 2026-08-28 (andrew's shadowing question ahead
   of ctx-as-map). The ctx-as-map swap must preserve this exactly; if
   the SEMANTICS is ever deliberately changed to def-site resolution,
   flip this test. */

let case = () => {
  let src = "type T = Int in\nlet x : T = 1 in\ntype T = Bool in\nx";
  switch (ParsedCorpus.to_segment(~root=Exp, src)) {
  | None => fail("unparseable")
  | Some(seg) =>
    let term = MakeTerm.go(seg).term;
    let (info_map, _) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        term,
      );
    let found = ref(false);
    Id.Map.iter(
      (_, info) =>
        switch ((info: Info.t)) {
        | InfoExp({user_term, elab_syn_ty, ctx, _}) =>
          switch (Exp.term_of(user_term)) {
          | Var("x") =>
            found := true;
            check(
              bool,
              "raw type is the unresolved alias reference",
              true,
              switch (Typ.term_of(elab_syn_ty)) {
              | Var("T") => true
              | _ => false
              },
            );
            check(
              bool,
              "use-site normalization resolves to the SHADOWING alias",
              true,
              switch (Typ.term_of(Typ.normalize(ctx, elab_syn_ty))) {
              | Atom(Bool) => true
              | _ => false
              },
            );
          | _ => ()
          }
        | _ => ()
        },
      info_map,
    );
    check(bool, "found the use of x", true, found^);
  };
};

let tests = (
  "AliasProbe",
  [test_case("shadowed alias resolves at use site", `Quick, case)],
);
