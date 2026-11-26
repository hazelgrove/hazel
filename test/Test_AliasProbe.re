open Alcotest;
open Haz3lcore;
open Language;

/* characterization: a shadowing alias between binder and use is renamed
   (Typ.unshadow), so the alias in a variable's type keeps the meaning it
   had at the binder */

let case = () => {
  let src = "type T = Int in\nlet x : T = 1 in\ntype T = Bool in\nx";
  let term = MakeTerm.go(CorpusUtil.seg_of(src)).term;
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
            "use-site normalization resolves to the alias at the binder",
            true,
            switch (Typ.term_of(Typ.normalize(ctx, elab_syn_ty))) {
            | Atom(Int) => true
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

let tests = (
  "AliasProbe",
  [test_case("a shadowed alias keeps its meaning", `Quick, case)],
);
