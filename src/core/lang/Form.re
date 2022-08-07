open Sexplib.Std;
open Mold;
module P = Precedence;

/* FORM

   This module determines the syntactic extent of the language; the
   entire Syntax module is driven by the below definitions. Adding
   a new syntactic form is simply a matter of adding a new line to either
   the 'convex_monos' table, for single-token forms, or the 'forms'
   table, for compound forms.

   The wrapping functions seen in both of those tables determine the
   shape, precedence, and expansion behavior of the form. */

let regexp = (r, s) => Re.Str.string_match(Re.Str.regexp(r), s, 0);

/* A label is the textual expression of a form's delimiters */
[@deriving (show({with_path: false}), sexp, yojson)]
type label = list(Token.t);

/* The construction of a compound forms can be triggered by inserting
   one of its delimiters through a process called expansion. Expansion
   can either occur (Instant)ly upon delimiter creation, or be (Delayed)
   until after a token boundary event is triggered (say by pressing
   space after entering 'let'). The (Static) case is used for monos
   aka single-token forms. */
[@deriving (show({with_path: false}), sexp, yojson)]
type expansion_time =
  | Static
  | Instant
  | Delayed;

/* Expansion can be triggered by either/both the first or last token
   of a form, represented here by the first/last elements of this pair. */
[@deriving (show({with_path: false}), sexp, yojson)]
type expansion = (expansion_time, expansion_time);

/* A label, a mold, and expansion behavior together determine a form. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  label,
  expansion,
  mold: Mold.t,
};

let mk = (expansion, label, mold) => {label, mold, expansion};

/* Abbreviations for expansion behaviors */
let ss: expansion = (Static, Static);
let ii: expansion = (Instant, Instant);
let id: expansion = (Instant, Delayed);
let is: expansion = (Instant, Static);
let ds: expansion = (Delayed, Static);
let di: expansion = (Delayed, Instant);

let mk_infix = (t: Token.t, sort: Sort.t, prec) =>
  mk(ss, [t], mk_bin(prec, sort, []));

/* Regular expressions which determine monotiles */
let is_var = regexp("^[a-z][A-Za-z0-9_]*$");
let is_int = regexp("^[0-9]*$");
let is_float = x => x != "." && regexp("^[0-9]*\\.[0-9]*$", x);
let is_typ_lit = regexp("^(Int|Float|Bool)$");
let is_typ_lit_partial = x =>
  !is_typ_lit(x) && regexp("^[A-Z][A-Za-z0-9_]*$", x);
let is_wild = regexp("^_$");
let is_bool = regexp("^true|false$");

/* A. Whitespace: */
let whitespace = [Whitespace.space, Whitespace.linebreak];

/* B. Operands:
   Order in this list determines relative remolding
   priority for forms with overlapping regexps */
let convex_monos: list((string, (string => bool, list(Mold.t)))) = [
  ("var", (is_var, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("type", (is_typ_lit_partial, [mk_op(Typ, [])])),
  //("type-partial", (is_typ_lit_partial, [mk_op(Nul, [])])),
  ("float", (is_float, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("int", (is_int, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("wild", (is_wild, [mk_op(Pat, [])])),
];

/* C. Compound Forms:
   Order in this list determines relative remolding
   priority for forms which share the same labels */
let forms: list((string, t)) = [
  ("cell-join", mk_infix(";", Exp, 10)),
  ("plus", mk_infix("+", Exp, P.plus)),
  ("minus", mk_infix("-", Exp, P.plus)),
  ("times", mk_infix("*", Exp, P.mult)),
  ("divide", mk_infix("/", Exp, P.mult)),
  ("assign", mk_infix("=", Nul, P.eqs)), // HACK: SUBSTRING REQ
  ("equals", mk_infix("==", Exp, P.eqs)),
  ("lt", mk_infix("<", Exp, 5)), //TODO: precedence
  ("gt", mk_infix(">", Exp, 5)), //TODO: precedence
  //("not_equals", mk_infix("!=", Exp, 5)),
  //("gte", mk_infix("<=", Exp, P.eqs)),
  //("lte", mk_infix(">=", Exp, P.eqs)),
  ("fplus", mk_infix("+.", Exp, P.plus)),
  ("fminus", mk_infix("-.", Exp, P.plus)),
  ("ftimes", mk_infix("*.", Exp, P.mult)),
  ("fdivide", mk_infix("/.", Exp, P.mult)),
  ("fequals", mk_infix("==.", Exp, P.eqs)),
  ("flt", mk_infix("<.", Exp, 5)), //TODO: precedence
  ("fgt", mk_infix(">.", Exp, 5)), //TODO: precedence
  //("fnot_equals", mk_infix("!=.", Exp, 5)),
  //("fgte", mk_infix("<=.", Exp, P.eqs)),
  //("flte", mk_infix(">=.", Exp, P.eqs)),
  ("bitwise_and", mk_infix("&", Nul, 5)), // HACK: SUBSTRING REQ
  ("logical_and", mk_infix("&&", Exp, 5)),
  //("bitwise_or", mk_infix("|", Exp, 5)),
  ("logical_or", mk_infix("||", Exp, 6)),
  ("dot", mk(ss, ["."], mk_op(Nul, []))), // HACK: SUBSTRING REQ (floats)
  //("unary_minus", mk(ss, ["-"], mk_pre(P.fact, Exp, []))),
  ("comma_exp", mk_infix(",", Exp, P.prod)),
  ("comma_pat", mk_infix(",", Pat, P.prod)),
  ("comma_typ", mk_infix(",", Typ, P.prod)),
  ("type-arrow", mk_infix("->", Typ, 6)),
  ("parens_exp", mk(ii, ["(", ")"], mk_op(Exp, [Exp]))),
  ("parens_pat", mk(ii, ["(", ")"], mk_op(Pat, [Pat]))),
  (
    "funann",
    mk(ds, ["funann", ":", "->"], mk_pre(P.let_, Exp, [Pat, Typ])),
  ),
  ("fun_", mk(ds, ["fun", "->"], mk_pre(P.let_, Exp, [Pat]))),
  ("if_", mk(di, ["if", "then", "else"], mk_pre(P.if_, Exp, [Exp, Exp]))),
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
  ("typeann", mk(ss, [":"], mk_bin'(P.ann, Pat, Pat, [], Typ))),
  (
    "case",
    mk(ds, ["case", "of"], mk_pre'(P.case_, Exp, Exp, [Exp], Rul)),
  ),
  ("rule_arr", mk(ss, ["=>"], mk_bin'(P.rule_arr, Rul, Pat, [], Exp))),
  ("rule_sep", mk_infix("|", Rul, P.rule_sep)),
  //("rule_sep_pre", mk(ss, ["|"], mk_pre(P.let_, Rul, []))),
  ("test", mk(ds, ["test", "end"], mk_op(Exp, [Exp]))),
  //("concat", mk_infix("@", Exp, P.concat)),
  //("rev_ap", mk_infix("|>", Exp, P.eqs)),
  //("cons", mk_infix("::", Exp, 5)),
  //("fact", mk(ss, ["!"], mk_post(P.fact, Exp, []))),
  //("array_access", mk(ii, ["[", "]"], mk_post(P.ap, Exp, [Exp]))),
  //("list_lit", mk(ii, ["[", "]"], mk_op(Exp, [Exp]))),
  //("cond", mk(is, ["?", ":"], mk_bin(P.cond, Exp, [Exp]))),
  //("block", mk(ii, ["{", "}"], mk_op(Exp, [Exp]))),
];

let get: String.t => t = name => List.assoc(name, forms);

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
