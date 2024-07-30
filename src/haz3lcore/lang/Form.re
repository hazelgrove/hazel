open Util;
open StringUtil;
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

[@deriving (show({with_path: false}), sexp, yojson)]
type bad_token_cls =
  | Other
  | BadInt;

let mk = (expansion, label, mold) => {label, mold, expansion};

/* Abbreviations for expansion behaviors */
let ss: expansion = (Static, Static);
let ii: expansion = (Instant, Instant);
let is: expansion = (Instant, Static);
let ds: expansion = (Delayed, Static);

let mk_infix = (t: Token.t, sort: Sort.t, prec) =>
  mk(ss, [t], mk_bin(prec, sort, []));

let mk_nul_infix = (t: Token.t, prec) =>
  mk(ss, [t], mk_bin(~l=Any, ~r=Any, prec, Any, []));

/* Token Recognition Predicates */

/* A. Secondary Notation (Comments, Whitespace, etc.)  */
let space = " ";
/* HACK(andrew): Using ⏎ char to represent linebreak to avoid regexp
   issues with using \n. Someone who understands regexps better
   should fix this. */
let linebreak = "⏎";
let comment_regexp = regexp("^#[^#⏎]*#$"); /* Multiline comments not supported */
let is_comment = t => match(comment_regexp, t) || t == "#";
let is_comment_delim = t => t == "#";
let is_secondary = t =>
  List.mem(t, [space, linebreak]) || match(comment_regexp, t);

/* STRINGS: special-case syntax */

/* is_string: last clause is a somewhat hacky way of making sure
   there are at most two quotes, in order to prevent merges */
let string_regexp = regexp("^\"[^⏎]*\"$");
let is_string = t =>
  match(string_regexp, t) && List.length(String.split_on_char('"', t)) < 4;
let string_delim = "\"";
let empty_string = string_delim ++ string_delim;
let is_string_delim = (==)(string_delim);
let strip_quotes = s =>
  if (String.length(s) < 2) {
    s;
  } else if (String.sub(s, 0, 1) != "\""
             || String.sub(s, String.length(s) - 1, 1) != "\"") {
    s;
  } else {
    String.sub(s, 1, String.length(s) - 2);
  };
let string_quote = s => "\"" ++ s ++ "\"";

let keywords = [
  "fun",
  "let",
  "in",
  "type",
  "case",
  "test",
  "if",
  "then",
  "else",
];
let reserved_keywords = ["of", "when", "with", "switch", "match"];
let keyword_regexp = regexp("^(" ++ String.concat("|", keywords) ++ ")$");
let is_keyword = match(keyword_regexp);

/* Potential tokens: These are fallthrough classes which determine
 * the behavior when inserting a character in contact with a token */
let is_potential_operand = match(regexp("^[a-zA-Z0-9_'\\.?]+$"));
/* Anything else is considered a potential operator, as long
 *  as it does not contain any whitespace, linebreaks, comment
 *  delimiters, string delimiters, or the instant expanding paired
 *  delimiters: ()[]| */
let potential_operator_regexp =
  regexp("^[^a-zA-Z0-9_'?\"#⏎\\s\\[\\]\\(\\)]+$");
let is_potential_operator = match(potential_operator_regexp);
let is_potential_token = t =>
  is_potential_operand(t)
  || is_potential_operator(t)
  || is_string(t)
  || is_comment(t);

let int_regexp = regexp("^-?\\d+[0-9_]*$");
let is_float = match(regexp("^-?[0-9]*\\.?[0-9]*((e|E)-?[0-9]*)?$"));
let is_arbitary_float = x => x != "." && x != "-" && is_float(x);
let is_int = str => match(int_regexp, str) && int_of_string_opt(str) != None;
/* NOTE: The is_arbitary_int check is necessary to prevent
   minuses from being parsed as part of the int token. */

let is_bad_int = str => match(int_regexp, str) && !is_int(str);

/* NOTE: As well as making is_float  disjoint from is_int,
   the is_arbitary_int  also prevents ints over int_max from being
   cast as floats. The is_arbitary_float check is necessary to prevent
   minuses from being parsed as part of the float token. */
let is_float = str =>
  !match(int_regexp, str)
  && is_arbitary_float(str)
  && float_of_string_opt(str) != None;
let is_bad_float = str => is_arbitary_float(str) && !is_float(str);
let bools = ["true", "false"];
let is_bool = match(regexp("^(" ++ String.concat("|", bools) ++ ")$"));
let undefined = "undefined";
let is_undefined = match(regexp("^" ++ undefined ++ "$"));

let var_regexp =
  regexp(
    {|(^[a-z_][A-Za-z0-9_']*$)|(^[A-Z][A-Za-z0-9_']*\.[a-z][A-Za-z0-9_']*$)|},
  );
let is_var = str =>
  !is_bool(str)
  && !is_undefined(str)
  && str != "_"
  //&& !is_keyword(str)
  && match(var_regexp, str);
let capitalized_name_regexp = regexp("^[A-Z][A-Za-z0-9_]*$");
let is_ctr = match(capitalized_name_regexp);
let base_typs = ["String", "Int", "Float", "Bool"];
let is_base_typ =
  match(regexp("^(" ++ String.concat("|", base_typs) ++ ")$"));
let is_typ_var = str => is_var(str) || match(capitalized_name_regexp, str);
let wild = "_";
let is_wild = match(regexp("^" ++ wild ++ "$"));

/* List literals */
let list_start = "[";
let list_end = "]";
let listlit_lbl = [list_start, list_end];
let empty_list = list_start ++ list_end;
let is_empty_list = (==)(empty_list);

/* Tuples */
let tuple_start = "(";
let tuple_end = ")";
let tuple_lbl = [tuple_start, tuple_end];
let empty_tuple = tuple_start ++ tuple_end;
let is_empty_tuple = (==)(empty_tuple);

/* These functions determine which forms can switch back and forth between
   mono and duotile forms, like list literals and tuples switching to/from
   the empty list and empty tuple. Technically this should be derivable from
   the language data; leaving that for a future refactor. */
let duosplits = (t: Token.t): Label.t =>
  switch () {
  | _ when is_empty_list(t) => listlit_lbl
  | _ when is_empty_tuple(t) => tuple_lbl
  | _ => []
  };

let duomerges = (lbl: Label.t): option(Label.t) =>
  switch () {
  | _ when lbl == listlit_lbl => Some([empty_list])
  | _ when lbl == tuple_lbl => Some([empty_tuple])
  | _ => None
  };

let const_mono_delims =
  base_typs @ bools @ [undefined, wild, empty_list, empty_tuple, empty_string];

let explicit_hole = "?";
let is_explicit_hole = t => t == explicit_hole;
let bad_token_cls: string => bad_token_cls =
  t =>
    switch () {
    | _ when is_bad_int(t) => BadInt
    | _ => Other
    };

/* B. Operands:
   Order in this list determines relative remolding
   priority for forms with overlapping regexps */
let atomic_forms: list((string, (string => bool, list(Mold.t)))) = [
  ("var", (is_var, [mk_op(Exp, []), mk_op(Pat, [])])),
  (
    "explicit_hole",
    (
      is_explicit_hole,
      [mk_op(Exp, []), mk_op(Pat, []), mk_op(Typ, []), mk_op(TPat, [])],
    ),
  ),
  ("wild", (is_wild, [mk_op(Pat, [])])),
  ("string", (is_string, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("int_lit", (is_int, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("float_lit", (is_float, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("bool_lit", (is_bool, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("undefined_lit", (is_undefined, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("empty_list", (is_empty_list, [mk_op(Exp, []), mk_op(Pat, [])])),
  (
    "empty_tuple",
    (is_empty_tuple, [mk_op(Exp, []), mk_op(Pat, []), mk_op(Typ, [])]),
  ),
  ("deferral", (is_wild, [mk_op(Exp, [])])),
  ("ty_var", (is_typ_var, [mk_op(Typ, [])])),
  ("ty_var_p", (is_typ_var, [mk_op(TPat, [])])),
  ("ctr", (is_ctr, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("type", (is_base_typ, [mk_op(Typ, [])])),
];

/* C. Compound Forms:
   Order in this list determines relative remolding
   priority for forms which share the same labels */

let forms: list((string, t)) = [
  // INFIX OPERATORS
  ("typ_plus", mk_infix("+", Typ, P.type_plus)),
  ("type-arrow", mk_infix("->", Typ, P.type_arrow)),
  ("cell-join", mk_infix(";", Exp, P.semi)),
  ("plus", mk_infix("+", Exp, P.plus)),
  ("minus", mk_infix("-", Exp, P.plus)),
  ("times", mk_infix("*", Exp, P.mult)),
  ("power", mk_infix("**", Exp, P.power)),
  ("fpower", mk_infix("**.", Exp, P.power)),
  ("divide", mk_infix("/", Exp, P.mult)),
  ("equals", mk_infix("==", Exp, P.eqs)),
  ("string_equals", mk_infix("$==", Exp, P.eqs)),
  ("string_concat", mk_infix("++", Exp, P.plus)),
  ("lt", mk_infix("<", Exp, P.eqs)),
  ("gt", mk_infix(">", Exp, P.eqs)),
  ("not_equals", mk_infix("!=", Exp, P.eqs)),
  ("gte", mk_infix(">=", Exp, P.eqs)),
  ("lte", mk_infix("<=", Exp, P.eqs)),
  ("fplus", mk_infix("+.", Exp, P.plus)),
  ("fminus", mk_infix("-.", Exp, P.plus)),
  ("ftimes", mk_infix("*.", Exp, P.mult)),
  ("fdivide", mk_infix("/.", Exp, P.mult)),
  ("fequals", mk_infix("==.", Exp, P.eqs)),
  ("flt", mk_infix("<.", Exp, P.eqs)),
  ("fgt", mk_infix(">.", Exp, P.eqs)),
  ("fnot_equals", mk_infix("!=.", Exp, P.eqs)),
  ("fgte", mk_infix(">=.", Exp, P.eqs)),
  ("flte", mk_infix("<=.", Exp, P.eqs)),
  ("logical_and", mk_infix("&&", Exp, P.and_)),
  ("logical_or_legacy", mk_infix("\\/", Exp, P.or_)),
  ("logical_or", mk_infix("||", Exp, P.or_)),
  ("list_concat", mk_infix("@", Exp, P.plus)),
  ("cons_exp", mk_infix("::", Exp, P.cons)),
  ("cons_pat", mk_infix("::", Pat, P.cons)),
  ("typeann", mk(ss, [":"], mk_bin'(P.ann, Pat, Pat, [], Typ))),
  // UNARY PREFIX OPERATORS
  ("not", mk(ii, ["!"], mk_pre(5, Exp, []))), //TODO: precedence
  ("typ_sum_single", mk(ss, ["+"], mk_pre(P.or_, Typ, []))),
  ("unary_minus", mk(ss, ["-"], mk_pre(P.neg, Exp, []))),
  ("unquote", mk(ss, ["$"], mk_pre(P.unquote, Exp, []))),
  // N-ARY OPS (on the semantics level)
  ("comma_exp", mk_infix(",", Exp, P.comma)),
  ("comma_pat", mk_infix(",", Pat, P.comma)),
  ("comma_typ", mk_infix(",", Typ, P.type_prod)),
  // PAIRED DELIMITERS:
  ("list_lit_exp", mk(ii, ["[", "]"], mk_op(Exp, [Exp]))),
  ("list_lit_pat", mk(ii, ["[", "]"], mk_op(Pat, [Pat]))),
  ("list_typ", mk(ii, ["[", "]"], mk_op(Typ, [Typ]))),
  //NOTE(andrew): parens being below aps is load-bearing, unfortunately
  ("parens_exp", mk(ii, ["(", ")"], mk_op(Exp, [Exp]))),
  ("parens_pat", mk(ii, ["(", ")"], mk_op(Pat, [Pat]))),
  ("parens_typ", mk(ii, ["(", ")"], mk_op(Typ, [Typ]))),
  ("ap_exp_empty", mk(ii, ["()"], mk_post(P.ap, Exp, []))),
  ("ap_exp", mk(ii, ["(", ")"], mk_post(P.ap, Exp, [Exp]))),
  ("ap_pat", mk(ii, ["(", ")"], mk_post(P.ap, Pat, [Pat]))),
  ("ap_typ", mk(ii, ["(", ")"], mk_post(P.ap, Typ, [Typ]))),
  (
    "ap_exp_typ",
    mk((Instant, Static), ["@<", ">"], mk_post(P.ap, Exp, [Typ])),
  ),
  ("at_sign", mk_nul_infix("@", P.eqs)), // HACK: SUBSTRING REQ
  ("case", mk(ds, ["case", "end"], mk_op(Exp, [Rul]))),
  ("test", mk(ds, ["test", "end"], mk_op(Exp, [Exp]))),
  ("fun_", mk(ds, ["fun", "->"], mk_pre(P.fun_, Exp, [Pat]))),
  ("typfun", mk(ds, ["typfun", "->"], mk_pre(P.fun_, Exp, [TPat]))),
  ("forall", mk(ds, ["forall", "->"], mk_pre(P.fun_, Typ, [TPat]))),
  ("rec", mk(ds, ["rec", "->"], mk_pre(P.fun_, Typ, [TPat]))),
  (
    "rule",
    mk(ds, ["|", "=>"], mk_bin'(P.rule_sep, Rul, Exp, [Pat], Exp)),
  ),
  ("pipeline", mk_infix("|>", Exp, P.eqs)), // in OCaml, pipeline precedence is in same class as '=', '<', etc.
  // DOUBLE DELIMITERS
  ("filter_hide", mk(ds, ["hide", "in"], mk_pre(P.let_, Exp, [Exp]))),
  ("filter_eval", mk(ds, ["eval", "in"], mk_pre(P.let_, Exp, [Exp]))),
  ("filter_pause", mk(ds, ["pause", "in"], mk_pre(P.let_, Exp, [Exp]))),
  ("filter_debug", mk(ds, ["debug", "in"], mk_pre(P.let_, Exp, [Exp]))),
  // TRIPLE DELIMITERS
  ("let_", mk(ds, ["let", "=", "in"], mk_pre(P.let_, Exp, [Pat, Exp]))),
  (
    "type_alias",
    mk(ds, ["type", "=", "in"], mk_pre(P.let_, Exp, [TPat, Typ])),
  ),
  ("if_", mk(ds, ["if", "then", "else"], mk_pre(P.if_, Exp, [Exp, Exp]))),
];

let get: String.t => t =
  name => Util.ListUtil.assoc_err(name, forms, "Forms.get");

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

let is_delim = t => List.mem(t, delims);

let is_valid_token = t => is_atomic(t) || is_secondary(t) || is_delim(t);

let mk_atomic = (sort: Sort.t, t: Token.t) => {
  assert(is_atomic(t));
  mk(ss, [t], Mold.(mk_op(sort, [])));
};
