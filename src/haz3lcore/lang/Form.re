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
   shape, precedence, and expansion behavior of the form.
   */

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

let mk = (expansion, label, mold) => {
  label,
  mold,
  expansion,
};

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
let linebreak = "\n";
let comment_regexp = regexp("^#[^#\n]*#$"); /* Multiline comments not supported */
let is_comment = t => match(comment_regexp, t) || t == "#";
let is_comment_delim = t => t == "#";
let is_secondary = t =>
  List.mem(t, [space, linebreak]) || match(comment_regexp, t);

/* STRINGS: special-case syntax */

/* is_string: last clause is a somewhat hacky way of making sure
   there are at most two quotes, in order to prevent merges */
let string_regexp = regexp("^\"[^\n]*\"$"); /* Multiline strings not supported */
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
  "hint",
];
let reserved_keywords = ["of", "when", "with", "switch", "match"];
let keyword_regexp = regexp("^(" ++ String.concat("|", keywords) ++ ")$");
let is_keyword = match(keyword_regexp);

/* Potential tokens: These are fallthrough classes which determine
 * the behavior when inserting a character in contact with a token */
let is_potential_operand =
  match(regexp("^([a-zA-Z0-9_'?\\^]+)$|^([0-9_]+\\.[a-zA-Z0-9_'\\.?]*)$"));
/* Anything else is considered a potential operator, as long
 *  as it does not contain any whitespace, linebreaks, comment
 *  delimiters, string delimiters, or the instant expanding paired
 *  delimiters: ()[]| */
let potential_operator_regexp =
  regexp("^[^a-zA-Z0-9_'?\\^\"#\n\\s\\[\\]\\(\\)]+$"); /* Multiline operators not supported */
let is_potential_operator = match(potential_operator_regexp);
let begins_with_potential_operator =
  match(regexp("^[^a-zA-Z0-9_'?\"#\n\\s\\[\\]\\(\\)]+"));
let is_potential_token = t =>
  is_potential_operand(t)
  || is_potential_operator(t)
  || is_string(t)
  || is_comment(t);

let int_regexp = regexp("^-?\\d+[0-9_]*$");
let is_float = match(regexp("^-?[0-9]*\\.?[0-9]*((e|E)-?[0-9]*)?$"));
let is_arbitary_float = x => x != "." && x != "-" && is_float(x);
let is_int = str =>
  match(int_regexp, str) && Bigint.of_string_opt(str) != None;
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

let is_livelit = str => {
  let re = regexp("^(\\^)([a-z][A-Za-z0-9_]*)$");
  let result = match(re, str);
  result;
};
let parse_livelit = (str): string =>
  if (String.length(str) > 1 && String.sub(str, 0, 1) == "^") {
    String.sub(str, 1, String.length(str) - 1);
  } else {
    "invalid form";
  };

let var_regexp =
  regexp(
    {|(^[a-z_][A-Za-z0-9_']*$)|(^[A-Z][A-Za-z0-9_']*\.[a-z][A-Za-z0-9_']*$)|},
  );
let is_var = str =>
  !is_bool(str)
  && !is_undefined(str)
  && !is_livelit(str)
  && str != "_"
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

/* Module literals */
let module_start = "{";
let module_end = "}";
let modulelit_lbl = [module_start, module_end];
let empty_module = module_start ++ module_end;
let is_empty_module = (==)(empty_module);
let is_empty_module_signature = (==)(empty_module);

/* These functions determine which forms can switch back and forth between
   mono and duotile forms, like list literals and tuples switching to/from
   the empty list and empty tuple. Technically this should be derivable from
   the language data; leaving that for a future refactor. */
let duosplits = (t: Token.t): Label.t =>
  switch () {
  | _ when is_empty_list(t) => listlit_lbl
  | _ when is_empty_tuple(t) => tuple_lbl
  | _ when is_empty_module(t) => modulelit_lbl
  | _ => []
  };

let duomerges = (lbl: Label.t): option(Label.t) =>
  switch () {
  | _ when lbl == listlit_lbl => Some([empty_list])
  | _ when lbl == tuple_lbl => Some([empty_tuple])
  | _ when lbl == modulelit_lbl => Some([empty_module])
  | _ => None
  };

let const_mono_delims =
  base_typs
  @ bools
  @ [undefined, wild, empty_list, empty_tuple, empty_string, empty_module];

let explicit_hole = "?";
let llm_hole = "??";
let llm_advanced_reasoning_hole = "?a";
let is_explicit_hole = t => t == explicit_hole;
let is_llm_hole = t => t == llm_hole || t == llm_advanced_reasoning_hole;

let bad_token_cls: string => bad_token_cls =
  t =>
    switch () {
    | _ when is_bad_int(t) => BadInt
    | _ => Other
    };

let mk_parens = (sort: Sort.t) => mk(ii, tuple_lbl, mk_op(sort, [sort]));

/* B. Operands:
   Order in this type determines relative remolding
   priority for forms with overlapping regexps */

[@deriving enumerate]
type atomic_form =
  | Var
  | ExplicitHole
  | LLMHole
  | Wild
  | String
  | IntLit
  | FloatLit
  | BoolLit
  | LivelitName
  | UndefinedLit
  | EmptyList
  | EmptyTuple
  | EmptyModule
  | EmptyModuleSignature
  | Deferral
  | TyVar
  | TyVarP
  | Ctr
  | Type;

let get_atomic_form: atomic_form => (string => bool, list(Mold.t)) =
  fun
  | Var => (is_var, [mk_op(Exp, []), mk_op(Pat, [])])
  | ExplicitHole => (
      is_explicit_hole,
      [mk_op(Exp, []), mk_op(Pat, []), mk_op(Typ, []), mk_op(TPat, [])],
    )
  | LLMHole => (
      is_llm_hole,
      [mk_op(Exp, []), mk_op(Pat, []), mk_op(Typ, []), mk_op(TPat, [])],
    )
  | Wild => (is_wild, [mk_op(Pat, [])])
  | String => (is_string, [mk_op(Exp, []), mk_op(Pat, [])])
  | IntLit => (is_int, [mk_op(Exp, []), mk_op(Pat, [])])
  | FloatLit => (is_float, [mk_op(Exp, []), mk_op(Pat, [])])
  | LivelitName => (is_livelit, [mk_op(Exp, []), mk_op(Pat, [])])
  | BoolLit => (is_bool, [mk_op(Exp, []), mk_op(Pat, [])])
  | UndefinedLit => (is_undefined, [mk_op(Exp, []), mk_op(Pat, [])])
  | EmptyList => (is_empty_list, [mk_op(Exp, []), mk_op(Pat, [])])
  | EmptyTuple => (is_empty_tuple, [mk_op(Exp, [])])
  | EmptyModule => (is_empty_module, [mk_op(Exp, []), mk_op(Pat, [])])
  | EmptyModuleSignature => (is_empty_module_signature, [mk_op(Typ, [])])
  | Deferral => (is_wild, [mk_op(Exp, [])])
  | TyVar => (is_typ_var, [mk_op(Typ, [])])
  | TyVarP => (is_typ_var, [mk_op(TPat, [])])
  | Ctr => (is_ctr, [mk_op(Exp, []), mk_op(Pat, [])])
  | Type => (is_base_typ, [mk_op(Typ, [])]);

let atomic_forms: list((atomic_form, (string => bool, list(Mold.t)))) =
  List.map(f => (f, get_atomic_form(f)), all_of_atomic_form);

/* C. Compound Forms:
   Order in this type determines relative remolding
   priority for forms which share the same labels
   A list of all possible compound_forms is automatically
   generated by @deriving enumerate */

[@deriving enumerate]
type compound_form =
  // INFIX OPERATORS
  | TypeArrow
  | CellJoin
  | Plus
  | Minus
  | Times
  | Power
  | FPower
  | Divide
  | Equals
  | StringEquals
  | StringConcat
  | Lt
  | Gt
  | NotEquals
  | Gte
  | Lte
  | FPlus
  | FMinus
  | FTimes
  | FDivide
  | FEquals
  | FLt
  | FGt
  | FNotEquals
  | FGte
  | FLte
  | LogicalAnd
  | LogicalOrLegacy
  | LogicalOr
  | ListConcat
  | ConsExp
  | ConsPat
  | Typeann
  | TupleLabeledExp
  | TupleLabeledPat
  | TupleLabeledTyp
  | DotExp
  | DotTyp
  | TypeAsc
  | TypPlus
  // UNARY PREFIX OPERATORS
  | Not
  | TypSumSingle
  | UnaryMinus
  | Unquote
  // N-ARY OPS (on the semantics level)
  | CommaExp
  | CommaPat
  | CommaTyp
  // PAIRED DELIMITERS:
  | ListLitExp
  | ListLitPat
  | ListTyp
  //NOTE(andrew): parens being below aps is load-bearing, unfortunately
  | ParensExp
  | ParensPat
  | ParensTyp
  | ApExpEmpty
  | ApExp
  | ApPat
  | ApTyp
  | ApExpTyp
  | AtSign
  | Case
  | Test
  | HintedTest
  | Fun
  | Fix
  | TypFun
  | Forall
  | Rec
  | Rule
  | Pipeline
  // DOUBLE DELIMITERS
  | FilterHide
  | FilterEval
  | FilterPause
  | FilterDebug
  | Use
  // TRIPLE DELIMITERS
  | Let
  | TypeAlias
  | If
  // Modules
  | ModuleExp
  | ModuleSignature
  | ValBinding
  | ValType
  | TypeBinding
  | SignatureTypeBinding
  | ModuleEntryJoin
  | ModuleSignatureEntryJoin;

let get: compound_form => t =
  fun
  // INFIX OPERATORS
  | TypeArrow => mk_infix("->", Typ, P.type_arrow)
  | CellJoin => mk_infix(";;", Exp, P.semi)
  | Plus => mk_infix("+", Exp, P.plus)
  | Minus => mk_infix("-", Exp, P.plus)
  | Times => mk_infix("*", Exp, P.mult)
  | Power => mk_infix("**", Exp, P.power)
  | FPower => mk_infix("**.", Exp, P.power)
  | Divide => mk_infix("/", Exp, P.mult)
  | Equals => mk_infix("==", Exp, P.eqs)
  | StringEquals => mk_infix("$==", Exp, P.eqs)
  | StringConcat => mk_infix("++", Exp, P.concat)
  | Lt => mk_infix("<", Exp, P.eqs)
  | Gt => mk_infix(">", Exp, P.eqs)
  | NotEquals => mk_infix("!=", Exp, P.eqs)
  | Gte => mk_infix(">=", Exp, P.eqs)
  | Lte => mk_infix("<=", Exp, P.eqs)
  | FPlus => mk_infix("+.", Exp, P.plus)
  | FMinus => mk_infix("-.", Exp, P.plus)
  | FTimes => mk_infix("*.", Exp, P.mult)
  | FDivide => mk_infix("/.", Exp, P.mult)
  | FEquals => mk_infix("==.", Exp, P.eqs)
  | FLt => mk_infix("<.", Exp, P.eqs)
  | FGt => mk_infix(">.", Exp, P.eqs)
  | FNotEquals => mk_infix("!=.", Exp, P.eqs)
  | FGte => mk_infix(">=.", Exp, P.eqs)
  | FLte => mk_infix("<=.", Exp, P.eqs)
  | LogicalAnd => mk_infix("&&", Exp, P.and_)
  | LogicalOrLegacy => mk_infix("\\/", Exp, P.or_)
  | LogicalOr => mk_infix("||", Exp, P.or_)
  | ListConcat => mk_infix("@", Exp, P.concat)
  | ConsExp => mk_infix("::", Exp, P.cons)
  | ConsPat => mk_infix("::", Pat, P.cons)
  | Typeann => mk(ss, [":"], mk_bin'(P.asc, Pat, Pat, [], Typ))
  | TupleLabeledExp => mk_infix("=", Exp, P.lab)
  | TupleLabeledPat => mk_infix("=", Pat, P.lab)
  | TupleLabeledTyp => mk_infix("=", Typ, P.lab)
  | DotExp => mk_infix(".", Exp, P.dot)
  | DotTyp => mk_infix(".", Typ, P.dot)
  | TypeAsc => mk(ss, [":"], mk_bin'(P.asc, Exp, Exp, [], Typ))
  | TypPlus => mk_infix("+", Typ, P.type_plus)
  // UNARY PREFIX OPERATORS
  | Not => mk(ii, ["!"], mk_pre(P.not_, Exp, []))
  | TypSumSingle => mk(ss, ["+"], mk_pre(P.or_, Typ, []))
  | UnaryMinus => mk(ss, ["-"], mk_pre(P.neg, Exp, []))
  | Unquote => mk(ss, ["$"], mk_pre(P.unquote, Exp, []))
  // N-ARY OPS (on the semantics level)
  | CommaExp => mk_infix(",", Exp, P.comma)
  | CommaPat => mk_infix(",", Pat, P.comma)
  | CommaTyp => mk_infix(",", Typ, P.comma)
  // PAIRED DELIMITERS:
  | ListLitExp => mk(ii, ["[", "]"], mk_op(Exp, [Exp]))
  | ListLitPat => mk(ii, ["[", "]"], mk_op(Pat, [Pat]))
  | ListTyp => mk(ii, ["[", "]"], mk_op(Typ, [Typ]))
  //NOTE(andrew): parens being below aps is load-bearing, unfortunately
  | ParensExp => mk_parens(Exp)
  | ParensPat => mk_parens(Pat)
  | ParensTyp => mk_parens(Typ)
  | ApExpEmpty => mk(ii, ["()"], mk_post(P.ap, Exp, []))
  | ApExp => mk(ii, ["(", ")"], mk_post(P.ap, Exp, [Exp]))
  | ApPat => mk(ii, ["(", ")"], mk_post(P.ap, Pat, [Pat]))
  | ApTyp => mk(ii, ["(", ")"], mk_post(P.type_sum_ap, Typ, [Typ]))
  | ApExpTyp =>
    mk((Instant, Static), ["@<", ">"], mk_post(P.ap, Exp, [Typ]))
  | AtSign => mk_nul_infix("@", P.eqs) // HACK: SUBSTRING REQ
  | Case => mk(ds, ["case", "end"], mk_op(Exp, [Rul]))
  | Test => mk(ds, ["test", "end"], mk_op(Exp, [Exp]))
  | HintedTest => mk(ds, ["hint", "test", "end"], mk_op(Exp, [Exp, Exp]))
  | Fun => mk(ds, ["fun", "->"], mk_pre(P.fun_, Exp, [Pat]))
  | Fix => mk(ds, ["fix", "->"], mk_pre(P.fun_, Exp, [Pat]))
  | TypFun => mk(ds, ["typfun", "->"], mk_pre(P.fun_, Exp, [TPat]))
  | Forall => mk(ds, ["forall", "->"], mk_pre(P.fun_, Typ, [TPat]))
  | Rec => mk(ds, ["rec", "->"], mk_pre(P.fun_, Typ, [TPat]))
  | Rule => mk(ds, ["|", "=>"], mk_bin'(P.rule_sep, Rul, Exp, [Pat], Exp))
  | Pipeline => mk_infix("|>", Exp, P.eqs) // in OCaml, pipeline precedence is in same class as '=', '<', etc.
  // DOUBLE DELIMITERS
  | FilterHide => mk(ds, ["hide", "in"], mk_pre(P.let_, Exp, [Exp]))
  | FilterEval => mk(ds, ["eval", "in"], mk_pre(P.let_, Exp, [Exp]))
  | FilterPause => mk(ds, ["pause", "in"], mk_pre(P.let_, Exp, [Exp]))
  | FilterDebug => mk(ds, ["debug", "in"], mk_pre(P.let_, Exp, [Exp]))
  | Use => mk(ds, ["use", "in"], mk_pre(P.let_, Exp, [Typ]))
  // TRIPLE DELIMITERS
  | Let => mk(ds, ["let", "=", "in"], mk_pre(P.let_, Exp, [Pat, Exp]))
  | ModuleExp => mk(ii, ["{", "}"], mk_op(Exp, [ModuleEntry]))
  | ModuleSignature => {
      mk(ii, ["{", "}"], mk_op(Typ, [ModuleSignatureEntry]));
    }
  | TypeAlias =>
    mk(ds, ["type", "=", "in"], mk_pre(P.let_, Exp, [TPat, Typ]))
  | ValBinding =>
    mk(
      ds,
      ["val", "="],
      mk_pre'(P.let_, ModuleEntry, ModuleEntry, [Pat], Exp),
    )
  | ValType =>
    mk(
      ds,
      ["tval", ":"],
      mk_pre'(
        P.let_,
        ModuleSignatureEntry,
        ModuleSignatureEntry,
        [Pat],
        Typ,
      ),
    )
  | TypeBinding =>
    mk(
      ds,
      ["typedef", "="],
      mk_pre'(P.let_, ModuleEntry, ModuleEntry, [TPat], Typ),
    )
  | SignatureTypeBinding =>
    mk(
      ds,
      ["ttypedef", "="],
      mk_pre'(
        P.let_,
        ModuleSignatureEntry,
        ModuleSignatureEntry,
        [TPat],
        Typ,
      ),
    )
  | ModuleEntryJoin => mk_infix(";;", ModuleEntry, P.min)
  | ModuleSignatureEntryJoin => mk_infix(";;;", ModuleSignatureEntry, P.min)
  | If => mk(ds, ["if", "then", "else"], mk_pre(P.if_, Exp, [Exp, Exp]));

let forms: list((compound_form, t)) =
  List.map(f => (f, get(f)), all_of_compound_form);

let delims: list(Token.t) =
  forms
  |> List.fold_left((acc, (_, {label, _}: t)) => {label @ acc}, [])
  |> List.sort_uniq(compare);

let atomic_molds: Token.t => list(Mold.t) =
  s => {
    List.fold_left(
      (acc, (_, (test, molds))) => test(s) ? molds @ acc : acc,
      [],
      atomic_forms,
    );
  };

let is_atomic = t => {
  atomic_molds(t) != [];
};

let is_delim = t => List.mem(t, delims);

let is_valid_token = t => {
  is_atomic(t) || is_secondary(t) || is_delim(t);
};

let mk_atomic = (sort: Sort.t, t: Token.t) => {
  assert(is_atomic(t));
  mk(ss, [t], Mold.(mk_op(sort, [])));
};
