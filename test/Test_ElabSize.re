/* An elaboration carries no whitespace or comments: nothing reads them
   there, and they were about a third of each program sent to the eval
   worker. */
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
  [test_case("an elaboration has no secondary", `Quick, no_secondary)],
);
