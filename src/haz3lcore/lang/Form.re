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

let mk_nul_infix = (t: Token.t, prec) =>
  mk(ss, [t], mk_bin(~l=Any, ~r=Any, prec, Any, []));

/* Token Recognition Predicates */
let is_arbitary_int = regexp("^[0-9_]*$");
let is_arbitary_float = x => x != "." && regexp("^[0-9]*\\.[0-9]*$", x);
let is_int = str => is_arbitary_int(str) && int_of_string_opt(str) != None;
/* NOTE: The is_arbitary_int check is necessary to prevent
   minuses from being parsed as part of the int token. */

let is_bad_int = str => is_arbitary_int(str) && !is_int(str);

/* NOTE: As well as making is_float  disjoint from is_int,
   the is_arbitary_int  also prevents ints over int_max from being
   cast as floats. The is_arbitary_float check is necessary to prevent
   minuses from being parsed as part of the float token. */
let is_float = str =>
  !is_arbitary_int(str)
  && is_arbitary_float(str)
  && float_of_string_opt(str) != None;
let is_bad_float = str => is_arbitary_float(str) && !is_float(str);
let is_triv = str => str == "triv";
let is_undefined = str => str == "undefined";
let is_bool = str => str == "true" || str == "false";
let is_listnil = str => str == "nil";
let is_reserved = str =>
  is_listnil(str) || is_bool(str) || is_triv(str) || is_undefined(str);
let is_var = str => !is_reserved(str) && regexp("^[a-z][A-Za-z0-9_]*$", str);
let is_capitalized_name = regexp("^[A-Z][A-Za-z0-9_]*$");
let is_tag = is_capitalized_name;
let is_typ_var = is_capitalized_name;
let is_concrete_typ = str =>
  str == "String"
  || str == "Int"
  || str == "Float"
  || str == "Bool"
  || str == "Unit";
let is_partial_concrete_typ = x =>
  !is_concrete_typ(x) && is_capitalized_name(x);
let is_wild = regexp("^_$");
/* The below case represents tokens which we want the user to be able to
   type in, but which have no reasonable semantic interpretation */
let is_bad_lit = str =>
  is_bad_int(str) || is_bad_float(str) || is_partial_concrete_typ(str);

/* is_string: last clause is a somewhat hacky way of making sure
   there are at most two quotes, in order to prevent merges */
let is_string = t =>
  regexp("^\".*\"$", t) && List.length(String.split_on_char('"', t)) < 4;
let string_delim = "\"";
let is_string_delim = str => str == string_delim;

/* Whitelist: A regexp determining any other chars, not occuring in specific forms,
   which we want to let through. right now, this means that we'll be able to use
   them in strings/comments/any other free text forms. Currently these will cause
   exceptions when used elsewhere, as no molds will be found. Such exceptions are
   currently caught. This should be replaced by a more disciplined
   approach to invalid text.*/
let is_whitelisted_char = regexp("[!@]");

/* A. Secondary Notation (Comments, Whitespace, etc.)
   This list is for non-comments: */
let secondary_without_comments = [Secondary.space, Secondary.linebreak];

/* B. Operands:
   Order in this list determines relative remolding
   priority for forms with overlapping regexps */
let atomic_forms: list((string, (string => bool, list(Mold.t)))) = [
  ("bad_lit", (is_bad_lit, [mk_op(Any, [])])),
  ("var", (is_var, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("ty_var", (is_typ_var, [mk_op(Typ, [])])),
  ("ctr", (is_tag, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("type", (is_concrete_typ, [mk_op(Typ, [])])),
  ("unit_lit", (is_triv, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("undefined_lit", (is_undefined, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("bool_lit", (is_bool, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("float_lit", (is_float, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("int_lit", (is_int, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("wild", (is_wild, [mk_op(Pat, [])])),
  ("listnil", (is_listnil, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("string", (is_string, [mk_op(Exp, []), mk_op(Pat, [])])),
];

/* C. Compound Forms:
   Order in this list determines relative remolding
   priority for forms which share the same labels */

let forms: list((string, t)) = [
  ("cell-join", mk_infix(";", Exp, 10)),
  ("plus", mk_infix("+", Exp, P.plus)),
  ("minus", mk_infix("-", Exp, P.plus)),
  ("times", mk_infix("*", Exp, P.mult)),
  ("power", mk_infix("**", Exp, P.power)),
  ("fpower", mk_infix("**.", Exp, P.power)),
  ("divide", mk_infix("/", Exp, P.mult)),
  ("assign", mk_nul_infix("=", P.eqs)), // HACK: SUBSTRING REQ
  ("equals", mk_infix("==", Exp, P.eqs)),
  ("string_equals", mk_infix("$==", Exp, P.eqs)),
  ("string_equals_", mk_nul_infix("$=", P.eqs)), // HACK: SUBSTRING REQ
  ("string_equals__", mk_nul_infix("$", P.eqs)), // HACK: SUBSTRING REQ
  ("lt", mk_infix("<", Exp, 5)), //TODO: precedence
  ("gt", mk_infix(">", Exp, 5)), //TODO: precedence
  //("not_equals", mk_infix("!=", Exp, 5)),
  ("gte", mk_infix(">=", Exp, P.eqs)),
  ("lte", mk_infix("<=", Exp, P.eqs)),
  ("fplus", mk_infix("+.", Exp, P.plus)),
  ("fminus", mk_infix("-.", Exp, P.plus)),
  ("ftimes", mk_infix("*.", Exp, P.mult)),
  ("fdivide", mk_infix("/.", Exp, P.mult)),
  ("fequals", mk_infix("==.", Exp, P.eqs)),
  ("flt", mk_infix("<.", Exp, 5)), //TODO: precedence
  ("fgt", mk_infix(">.", Exp, 5)), //TODO: precedence
  //("fnot_equals", mk_infix("!=.", Exp, 5)),
  ("fgte", mk_infix(">=.", Exp, P.eqs)),
  ("flte", mk_infix("<=.", Exp, P.eqs)),
  ("substr1", mk_nul_infix("=.", P.eqs)), // HACK: SUBSTRING REQ
  ("bitwise_and", mk_nul_infix("&", P.and_)), // HACK: SUBSTRING REQ
  ("logical_and", mk_infix("&&", Exp, P.and_)),
  //("bitwise_or", mk_infix("|", Exp, 5)),
  ("logical_or", mk_infix("||", Exp, P.or_)),
  ("dot", mk(ss, ["."], mk_op(Any, []))), // HACK: SUBSTRING REQ (floats)
  ("unary_minus", mk(ss, ["-"], mk_pre(P.neg, Exp, []))),
  ("comma_exp", mk_infix(",", Exp, P.prod)),
  ("comma_pat", mk_infix(",", Pat, P.prod)),
  ("comma_typ", mk_infix(",", Typ, P.prod)),
  ("type-arrow", mk_infix("->", Typ, 6)),
  ("parens_exp", mk(ii, ["(", ")"], mk_op(Exp, [Exp]))),
  ("parens_pat", mk(ii, ["(", ")"], mk_op(Pat, [Pat]))),
  ("parens_typ", mk(ii, ["(", ")"], mk_op(Typ, [Typ]))),
  ("fun_", mk(ds, ["fun", "->"], mk_pre(P.fun_, Exp, [Pat]))),
  ("if_", mk(ds, ["if", "then", "else"], mk_pre(P.if_, Exp, [Exp, Exp]))),
  ("ap_exp", mk(ii, ["(", ")"], mk_post(P.ap, Exp, [Exp]))),
  ("ap_pat", mk(ii, ["(", ")"], mk_post(P.ap, Pat, [Pat]))),
  ("let_", mk(ds, ["let", "=", "in"], mk_pre(P.let_, Exp, [Pat, Exp]))),
  ("typeann", mk(ss, [":"], mk_bin'(P.ann, Pat, Pat, [], Typ))),
  ("case", mk(ds, ["case", "end"], mk_op(Exp, [Rul]))),
  (
    "rule",
    mk(
      ii,
      ["|", "=>"],
      {
        out: Rul,
        in_: [Pat],
        nibs: (
          {sort: Exp, shape: Concave(P.rule_sep)},
          {sort: Exp, shape: Concave(P.rule_sep)},
        ),
      },
    ),
  ),
  // ("rule_pre", mk(ss, ["|"], mk_pre(P.rule_pre, Rul, []))),
  // ("rule_sep", mk_infix("|", Rul, P.rule_sep)),
  ("test", mk(ds, ["test", "end"], mk_op(Exp, [Exp]))),
  //("concat", mk_infix("@", Exp, P.concat)),
  //("rev_ap", mk_infix("|>", Exp, P.eqs)),
  ("cons_exp", mk_infix("::", Exp, P.cons)),
  ("cons_pat", mk_infix("::", Pat, P.cons)),
  ("list_lit_exp", mk(ii, ["[", "]"], mk_op(Exp, [Exp]))),
  ("list_lit_pat", mk(ii, ["[", "]"], mk_op(Pat, [Pat]))),
  ("list_typ", mk(ii, ["[", "]"], mk_op(Typ, [Typ]))),
  //("fact", mk(ss, ["!"], mk_post(P.fact, Exp, []))),
  //("array_access", mk(ii, ["[", "]"], mk_post(P.ap, Exp, [Exp]))),
  //("cond", mk(is, ["?", ":"], mk_bin(P.cond, Exp, [Exp]))),
  //("block", mk(ii, ["{", "}"], mk_op(Exp, [Exp]))),
];

let get: String.t => t = name => List.assoc(name, forms);

let delims: list(Token.t) =
  forms
  |> List.fold_left((acc, (_, {label, _}: t)) => {label @ acc}, [])
  |> List.sort_uniq(compare);

let atomic_molds: Token.t => list(Mold.t) =
  s =>
    List.fold_left(
      (acc, (_, (test, molds))) => test(s) ? molds @ acc : acc,
      [],
      atomic_forms,
    );

let is_atomic = t => atomic_molds(t) != [];
let is_secondary = t =>
  List.mem(t, secondary_without_comments)
  || Re.Str.string_match(Secondary.comment, t, 0);

let is_comment = t =>
  Re.Str.string_match(Secondary.comment, t, 0) || t == "#";
let is_comment_delim = t => t == "#";

let is_delim = t => List.mem(t, delims);

let is_valid_token = t => is_atomic(t) || is_secondary(t) || is_delim(t);

let is_valid_char = t =>
  is_valid_token(t)
  || is_string_delim(t)
  || is_comment_delim(t)
  || is_whitelisted_char(t); //TODO(andrew): betterify this

let mk_atomic = (sort: Sort.t, t: Token.t) => {
  assert(is_atomic(t));
  mk(ss, [t], Mold.(mk_op(sort, [])));
};
