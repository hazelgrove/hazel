open Util;
open Language;

/* What Z3 may spend on one comparison, in its own units of work. */
let work_limit = 200000;

/* Whether Z3 finds two arithmetic expressions equal wherever their
 * divisors are nonzero, reading numbers as reals and a part it can't
 * translate as an unknown number, the same unknown for equal parts. The
 * stepper's search asks this before it tries an `algebra` step, which the
 * checker then checks by its own, stricter rules. */
let check_rewrite = (from_: Exp.t, to_: Exp.t): bool => {
  let unknowns: ref(list((Exp.t, string))) = ref([]);
  let unknown = (e: Exp.t): string =>
    switch (ListUtil.assoc_opt_by(Equality.semantic.exp, e, unknowns^)) {
    | Some(symbol) => symbol
    | None =>
      let symbol = "x" ++ string_of_int(List.length(unknowns^));
      unknowns := [(e, symbol), ...unknowns^];
      symbol;
    };
  let divisors = ref([]);
  let rec term = (e: Exp.t): string => {
    let apply = (op, a, b) =>
      "(" ++ op ++ " " ++ term(a) ++ " " ++ term(b) ++ ")";
    switch (Exp.term_of(e)) {
    | Atom(Int(n) | Nat(n)) when Bigint.(n >= zero) =>
      Bigint.to_string(n) ++ ".0"
    | Parens(e)
    | Asc(e, _) => term(e)
    | UnOp(Int(Minus), e) => "(- " ++ term(e) ++ ")"
    | BinOp(Int(Plus) | Nat(Plus), a, b) => apply("+", a, b)
    | BinOp(Int(Minus) | Nat(Minus), a, b) => apply("-", a, b)
    | BinOp(Int(Times) | Nat(Times), a, b) => apply("*", a, b)
    | BinOp(Int(Divide) | Nat(Divide), a, b) =>
      divisors := [term(b), ...divisors^];
      apply("/", a, b);
    | BinOp(Int(Power) | Nat(Power), a, {term: Atom(Int(n) | Nat(n)), _})
        when Bigint.(n >= one && n <= of_int(16)) =>
      "(* "
      ++ String.concat(
           ~sep=" ",
           List.init(Bigint.to_int_exn(n), ~f=_ => term(a)),
         )
      ++ ")"
    | _ => unknown(e)
    };
  };
  let from_ = term(DHExp.strip_ascriptions(from_));
  let to_ = term(DHExp.strip_ascriptions(to_));
  let declarations =
    List.map(unknowns^, ~f=((_, x)) => "(declare-const " ++ x ++ " Real)")
    |> String.concat;
  let nonzero =
    List.map(divisors^, ~f=d => "(assert (not (= " ++ d ++ " 0.0)))")
    |> String.concat;
  /* Z3's simplifier settles an identity between polynomials much faster
   * than its solver does. */
  let simplified =
    Z3.eval^(
      declarations ++ "(simplify (- " ++ from_ ++ " " ++ to_ ++ ") :som true)",
    );
  switch (Option.map(~f=String.strip, simplified)) {
  | None => false
  | Some("0.0") => true
  | Some(_) =>
    let answer =
      Z3.eval^(
        "(set-option :rlimit "
        ++ string_of_int(work_limit)
        ++ ")"
        ++ declarations
        ++ nonzero
        ++ "(assert (not (= "
        ++ from_
        ++ " "
        ++ to_
        ++ ")))(check-sat)",
      );
    switch (Option.map(~f=String.strip, answer)) {
    | Some("unsat") => true
    | Some(_)
    | None => false
    };
  };
};
