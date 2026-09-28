/* A builtin module's type embedded in an elaboration is written by its
   path, `Html.T`, not expanded: expanded it is a Rec of ~7800 nodes, about
   87 KB when the program is marshaled to the eval worker, and each copy was
   its own value. Every `[]` in a recursive Html builder carried one. */
open Alcotest;
open Language;

let elab = text =>
  switch (Haz3lcore.PersistentZipper.parse_text(~source="t", ~root=Exp, text)) {
  | None => fail("did not parse")
  | Some(z) =>
    let Haz3lcore.MakeTerm.{term, _} =
      Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
    snd(Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term));
  };

let size = v => String.length(Marshal.to_string(v, []));

let builder = "let f = fun n : Int -> if n > 5 then [] else Html.text(\"a\") :: f(n + 1) in f(0)";

/* Every ascription's type in e. */
let ascribed = (e: Exp.t): list(Typ.t) => {
  let found = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          switch (e.term) {
          | Asc(_, t) => found := [t, ...found^]
          | _ => ()
          };
          continue(e);
        },
      e,
    );
  found^;
};

let small = () => {
  let e = elab(builder);
  check(bool, "a few KB, not ~88", true, size(e) < 5000);
};

let by_path = () => {
  let e = elab(builder);
  let html_list =
    List.exists(
      (t: Typ.t) =>
        switch (Typ.term_of(t)) {
        | List(inner) =>
          switch (Typ.term_of(inner)) {
          | ProdProjection(_) => true
          | _ => false
          }
        | _ => false
        },
      ascribed(e),
    );
  check(bool, "[] is ascribed [Html.T]", true, html_list);
};

let runs = () => {
  let (result, _) =
    Evaluator.evaluate(~env=Builtins.env_init, elab(builder));
  let n =
    switch (DHExp.strip_ascriptions(result).term) {
    | ListLit(xs) => List.length(xs)
    | _ => (-1)
    };
  check(int, "six Text nodes", 6, n);
};

/* An elaboration carries no whitespace or comments: nothing reads them
   there, and they were about a third of each program sent to the eval
   worker. */
let commented = "# a comment #\nlet x =   # another #\n  1 + 2 in\n\n# and one more #\nx * 10";

let no_secondary = () => {
  let e = elab(commented);
  let with_secondary = ref(0);
  let count:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, x) => {
      if (x.annotation.secondary != IdTagged.IdTag.empty_secondary) {
        incr(with_secondary);
      };
      continue(x);
    };
  let _ = Exp.map_term(~f_exp=count, ~f_pat=count, e);

  check(int, "no expression or pattern keeps secondary", 0, with_secondary^);
  let (result, _) = Evaluator.evaluate(~env=Builtins.env_init, e);
  check(
    option(int),
    "and it still means 30",
    Some(30),
    switch (DHExp.strip_ascriptions(result).term) {
    | Atom(Int(n)) => Some(Bigint.to_int_exn(n))
    | _ => None
    },
  );
};

let tests = (
  "ElabSize",
  [
    test_case("an elaboration has no secondary", `Quick, no_secondary),
    test_case("a recursive Html builder is small", `Quick, small),
    test_case("its [] names Html.T", `Quick, by_path),
    test_case("and still runs", `Quick, runs),
  ],
);
