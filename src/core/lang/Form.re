open Sexplib.Std;
open Mold;
module P = Precedence;

let regexp = (r, s) => Re.Str.string_match(Re.Str.regexp(r), s, 0);

[@deriving (show({with_path: false}), sexp, yojson)]
type label = list(Token.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type expansion_time =
  | Static
  | Instant
  | Delayed;

[@deriving (show({with_path: false}), sexp, yojson)]
type expansion = (expansion_time, expansion_time);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  label,
  expansion,
  mold: Mold.t,
};

let ss: expansion = (Static, Static);
let ii: expansion = (Instant, Instant);
let id: expansion = (Instant, Delayed);
let is: expansion = (Instant, Static);
let ds: expansion = (Delayed, Static);
let di: expansion = (Delayed, Instant);

let mk = (expansion, label, mold) => {label, mold, expansion};

let mk_infix = (t: Token.t, sort: Sort.t, prec) =>
  mk(ss, [t], mk_bin(prec, sort, []));

let is_var = regexp("^[a-z][A-Za-z0-9_]*$");
let is_int = regexp("^[0-9]*$");
let is_typ_lit = regexp("^Int|Bool$");
let is_typ_lit_partial = regexp("^[A-Z][A-Za-z0-9_]*$");
let is_wild = regexp("^_$");
let is_bool = regexp("^true|false$");

/* A. Whitespace: */
let whitespace = [Whitespace.space, Whitespace.linebreak];

/* B. Operands:
   Order in this list determines relative remolding
   priority for forms with overlapping regexps */
let convex_monos: list((string, (string => bool, list(Mold.t)))) = [
  ("var", (is_var, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("type", (is_typ_lit, [mk_op(Exp, [])])), // bad sort
  ("type-partial", (is_typ_lit_partial, [mk_op(Typ, [])])), // bad sort (should be bottom)
  // TODO(andrew): above thing: color same as sort inconsistency errors
  // ("whatever", (regexp("#*$"), [mk_op(Nul, [])])),
  // ("whatever", (regexp("@*$"), [mk_op(Rul, [])])),
  ("num", (is_int, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("wild", (is_wild, [mk_op(Pat, [])])),
];

/* C. Compound Forms:
   Order in this list determines relative remolding
   priority for forms which share the same labels */
let forms: list((string, t)) = [
  ("plus", mk_infix("+", Exp, P.plus)),
  ("equals", mk_infix("=", Exp, P.eqs)),
  ("lt", mk_infix("<", Exp, P.eqs)),
  ("bitwise_and", mk_infix("&", Exp, 5)), // substring req
  ("logical_and", mk_infix("&&", Exp, 5)),
  ("type-arrow", mk_infix("->", Typ, 6)), // bad sorts
  ("unary_minus", mk(ss, ["-"], mk_pre(P.fact, Exp, []))), // substring req
  ("comma_exp", mk_infix(",", Exp, P.prod)),
  ("comma_pat", mk_infix(",", Pat, P.prod)),
  ("comma_typ", mk_infix(",", Typ, P.prod)),
  ("parens_exp", mk(ii, ["(", ")"], mk_op(Exp, [Exp]))), // construction req
  //("parens_pat", mk(ii, ["(", ")"], mk_op(Pat, [Pat]))),
  (
    "funann",
    mk(ds, ["funann", ":", "->"], mk_pre(P.let_, Exp, [Pat, Typ])),
  ),
  ("fun_", mk(ds, ["fun", "->"], mk_pre(P.let_, Exp, [Pat]))),
  ("if_", mk(di, ["if", "then", "else"], mk_pre(P.if_, Exp, [Exp, Exp]))),
  /* Something must instant on => as not valid monotile on its own */
  ("ap", mk(ii, ["(", ")"], mk_post(P.ap, Exp, [Exp]))),
  ("let_", mk(ds, ["let", "=", "in"], mk_pre(P.let_, Exp, [Pat, Exp]))),
  (
    "letann",
    mk(
      ds,
      ["letann", ":", "=", "in"],
      mk_pre(P.let_, Exp, [Pat, Typ, Exp]),
    ),
  ),
  //("times", mk_infix("*", Exp, P.mult)),
  //("divide", mk_infix("/", Exp, P.mult)),
  //("not_equals", mk_infix("!=", Exp, 5)),
  //("gt", mk_infix(">", Exp, P.eqs)),
  //("gte", mk_infix("<=", Exp, P.eqs)),
  //("lte", mk_infix(">=", Exp, P.eqs)),
  //("bitwise_or", mk_infix("|", Exp, 5)),
  //("logical_or", mk_infix("||", Exp, 5)),
  //("concat", mk_infix("@", Exp, P.concat)),
  //("rev_ap", mk_infix("|>", Exp, P.eqs)),
  //("cons", mk_infix("::", Exp, 5)),
  //("type-ann", mk_infix(":", Exp, 5)), // bad sorts
  //("dot-access", mk_infix(".", Exp, 5)), // bad sorts
  //("assign_incr", mk_infix("+=", Exp, 10)), // bad sorts
  //("minus", mk_infix("-", Exp, P.plus)),
  //("semi", mk_infix(";", Exp, P.semi)),
  //("fact", mk(ss, ["!"], mk_post(P.fact, Exp, []))),
  // ("array_access", mk(ii, ["[", "]"], mk_post(P.ap, Exp, [Exp]))),
  //("list_lit", mk(ii, ["[", "]"], mk_op(Exp, [Exp]))),
  //("cond", mk(is, ["?", ":"], mk_bin(P.cond, Exp, [Exp]))),
  //("block", mk(ii, ["{", "}"], mk_op(Exp, [Exp]))),
  //("case", mk(ds, ["case", "of"], mk_pre(9, Exp, [Exp]))),
  //("rule_first", mk(ds, ["|", "->"], mk_pre(9, Exp, [Pat]))),
  /* Something must instant on | as not valid monotile on its own */
  //("rule_rest", mk(ds, ["|", "->"], mk_bin(9, Exp, [Pat]))),
];

let get: Token.t => t = name => List.assoc(name, forms);

let delims: list(Token.t) =
  forms
  |> List.fold_left((acc, (_, {label, _}: t)) => {label @ acc}, [])
  |> List.sort_uniq(compare);

let convex_mono_molds: Token.t => list(Mold.t) =
  s =>
    List.fold_left(
      (acc, (_, (test, molds))) => test(s) ? molds @ acc : acc,
      [],
      convex_monos,
    );

let is_convex_mono = t => convex_mono_molds(t) != [];
let is_whitespace = t => List.mem(t, whitespace);
let is_delim = t => List.mem(t, delims);

let is_valid_token = t =>
  is_convex_mono(t) || is_whitespace(t) || is_delim(t);

let is_valid_char = is_valid_token; //TODO(andrew): betterify this

let mk_convex_mono = (sort: Sort.t, t: Token.t) => {
  assert(is_convex_mono(t));
  mk(ss, [t], Mold.(mk_op(sort, [])));
};
