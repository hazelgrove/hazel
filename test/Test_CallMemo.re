/* Language.CallMemo: a view's calls, reused across its evaluations. A
   function is elaborated once and applied to different inputs, as a
   livelit's definition is to a changed model. */
open Alcotest;
open Language;

let elab = text =>
  switch (Haz3lcore.PersistentZipper.parse_text(~source="t", ~root=Exp, text)) {
  | None => fail("did not parse")
  | Some(z) =>
    let Haz3lcore.MakeTerm.{term, _} =
      Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
    snd(
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        term,
      ),
    );
  };

let int_lit = (n: int): Exp.t => {
  let t: TermBase.Exp.term = Atom(Int(Bigint.of_int(n)));
  IdTagged.mk_internal([Id.mk()], t);
};

/* f applied to k, with or without a memo */
let apply = (~call_memo=?, f: Exp.t, k: int): Exp.t =>
  Evaluator.evaluate_open(
    ~call_memo?,
    ~env=Builtins.env_init,
    IdTagged.FreshGrammar.Exp.ap(Forward, f, int_lit(k)),
  );

let same_value = (a, b) =>
  Util.ValueHash.same(CallMemo.norm(a), CallMemo.norm(b));

let helpers = {|
let range = fun n -> if n == 0 then [] else n :: range(n - 1) in
let map = fun (f, xs) -> case xs | [] => [] | x :: t => f(x) :: map((f, t)) end in
let sum = fun xs -> case xs | [] => 0 | x :: t => x + sum(t) end in
|};

let tests = (
  "CallMemo",
  [
    test_case(
      "a call with the same inputs is reused, and gives what it gave",
      `Quick,
      () => {
        /* heavy(60) does not read k */
        let f =
          elab(
            "fun k -> "
            ++ helpers
            ++ "let heavy = fun n -> map((fun x -> (x, x * 2), range(n))) in (heavy(60), k)",
          );
        let memo = CallMemo.create();
        ignore(apply(~call_memo=memo, f, 1));
        let hits = memo.hits^;
        let reused = apply(~call_memo=memo, f, 2);
        check(bool, "reused something", true, memo.hits^ > hits);
        check(
          bool,
          "same as without",
          true,
          same_value(reused, apply(f, 2)),
        );
        check(
          bool,
          "and not the first answer",
          false,
          same_value(reused, apply(f, 1)),
        );
      },
    ),
    test_case(
      "a helper that reads a changed variable is evaluated again",
      `Quick,
      () => {
        /* heavy's argument is the same, but scale, which it calls, reads k */
        let f =
          elab(
            "fun k -> "
            ++ helpers
            ++ "let scale = fun x -> x * k in let heavy = fun n -> sum(map((scale, range(n)))) in heavy(60)",
          );
        let memo = CallMemo.create();
        let one = apply(~call_memo=memo, f, 1);
        let two = apply(~call_memo=memo, f, 2);
        check(bool, "k = 1", true, same_value(one, apply(f, 1)));
        check(bool, "k = 2", true, same_value(two, apply(f, 2)));
        check(bool, "they differ", false, same_value(one, two));
      },
    ),
    test_case(
      "the same input again reuses the whole call",
      `Quick,
      () => {
        let f =
          elab(
            "fun k -> "
            ++ helpers
            ++ "let heavy = fun n -> sum(range(n)) in heavy(60) + k",
          );
        let memo = CallMemo.create();
        let first = apply(~call_memo=memo, f, 3);
        let hits = memo.hits^;
        let again = apply(~call_memo=memo, f, 3);
        check(bool, "reused", true, memo.hits^ > hits);
        check(bool, "same value", true, same_value(first, again));
      },
    ),
    test_case(
      "small calls are not remembered",
      `Quick,
      () => {
        let f = elab("fun k -> let inc = fun x -> x + 1 in inc(inc(k))");
        let memo = CallMemo.create();
        ignore(apply(~call_memo=memo, f, 5));
        ignore(apply(~call_memo=memo, f, 5));
        check(int, "no reuse", 0, memo.hits^);
      },
    ),
    test_case(
      "ids are left out of a value's form, and a closure is its code and what it reads",
      `Quick,
      () => {
        let f = elab("fun k -> let g = fun x -> x + k in (g, [k, k + 1])");
        let a = apply(f, 4);
        let b = apply(f, 4);
        let c = apply(f, 5);
        check(bool, "fresh ids, same form", true, same_value(a, b));
        check(bool, "a different k", false, same_value(a, c));
      },
    ),
  ],
);
