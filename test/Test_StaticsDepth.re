/* Statics analyzes a let's body in a call that is not in tail position,
   so a chain of N lets is N nested frames, and what a frame costs decides
   how deep a program can go before the stack runs out. When every case of
   uexp_to_info_map lived in one switch, each frame held the locals of all
   of them: under the test runner's 8 MB stack a chain of 505 lets was the
   deepest that survived, and the browser, with far less, overflowed on the
   Kids' Choice face. With a function per case (f9b7d9357f) a cold run
   under the stock runner survives about 1,920, and a warmed-up one, whose
   frames the JIT has shrunk, about 12,070.

   1,000 is twice what the single switch allowed even warm, and about half
   the split's cold ceiling, which is how CI runs it. Frames growing back
   toward the old size fail this -- as a Stack_overflow, or, when node's
   stack is as large as the OS thread's (the stock runner), as a segfault.
   */
open Alcotest;
open Language;

let depth = 1000;

/* `let x0 = 0 in ... let x999 = 999 in 0`, built directly rather than
   parsed: parsing a chain this deep costs far more than analyzing it.
   Built inside out, iteratively, so building cannot overflow. Returns the
   innermost literal too, to check the analysis reached it. */
let chain = n => {
  let innermost = Exp.fresh(Atom(Int(Bigint.of_int(0))));
  let body = ref(innermost);
  for (i in n - 1 downto 0) {
    body :=
      Exp.fresh(
        Let(
          Pat.fresh(Var(Printf.sprintf("x%d", i))),
          Exp.fresh(Atom(Int(Bigint.of_int(i)))),
          body^,
        ),
      );
  };
  (body^, innermost);
};

let deep_let_chain = () => {
  let (term, innermost) = chain(depth);
  /* uexp_to_info_map, not Statics.mk: mk memoizes on the term. */
  let (_, _, m) =
    Statics.uexp_to_info_map(
      ~ctx=Builtins.ctx_init(Some(Int)),
      ~ancestors=[],
      term,
      Id.Map.empty,
    );
  check(
    bool,
    "the innermost literal was analyzed",
    true,
    Option.is_some(Statics.Map.lookup_exp(Exp.rep_id(innermost), m)),
  );
};

let tests = (
  "StaticsDepth",
  [
    test_case(
      Printf.sprintf("a chain of %d lets does not overflow", depth),
      `Quick,
      deep_let_chain,
    ),
  ],
);
