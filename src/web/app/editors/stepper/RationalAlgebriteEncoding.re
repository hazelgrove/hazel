open Util;
open Language;

/* Exact Algebrite text for Rational values and operators: the pieces the
 * existing printer (RewriteChecker.print_exp_for_algebrite) lacks. It
 * prints a Rational atom as an opaque `unknown_<hash>` name and Rational
 * `**` as `**`; Algebrite's power is `^`.
 *
 * NOT wired into that printer or checkEquality: integration waits for the
 * domain and CAS contract review, and nothing here makes any Rational
 * algebra available. No evaluation, normalization or decoding: the text
 * is read off the canonical value (lowest terms, positive denominator,
 * zero is 0/1; see Rational.rei). */

/* `(n/d)`, which Algebrite reads as the same exact rational, for every
   value: whole (`(5/1)`), negative (`(-3/4)`) and zero (`(0/1)`) alike.
   Parenthesized so it stays one operand. */
let rational = (r: Rational.t): string =>
  "("
  ++ Bigint.to_string(Rational.num(r))
  ++ "/"
  ++ Bigint.to_string(Rational.den(r))
  ++ ")";

/* None for other atoms: the existing printer keeps those. */
let atom = (a: Atom.t): option(string) =>
  switch (a) {
  | Rat(r) => Some(rational(r))
  | _ => None
  };

/* Algebrite's power is `^`; the other Rational operators use their shared
   token. None for non-Rational operators. */
let bin_op = (op: Operators.op_bin): option(string) =>
  switch (op) {
  | Rat(Power) => Some("^")
  | Rat(_) => Some(Operators.bin_op_to_string(op))
  | _ => None
  };

let un_op = (op: Operators.op_un): option(string) =>
  switch (op) {
  | Rat(Minus) => Some("-")
  | _ => None
  };
