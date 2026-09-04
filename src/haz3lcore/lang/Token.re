open Util_web;

// make an enum
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = string;

[@deriving (show({with_path: false}), sexp, yojson)]
type bad_token_cls =
  | Other
  | BadInt;

let compare = String.compare;
let equal = String.equal;
let concat = String.concat;
let starts_with = String.starts_with;
let split_on_char = String.split_on_char;
let sort_uniq = List.sort_uniq(compare);
let match = StringUtil.match;
let regexp = StringUtil.regexp;
let unicode_match = StringUtil.unicode_match;
let unicode_regexp = StringUtil.unicode_regexp;
let prefixes = StringUtil.prefixes;
let abbreviate = StringUtil.abbreviate;

let length = Unicode.length;
let append = Unicode.append;
let rm_nth = Unicode.remove_nth;
let rm_last = Unicode.remove_last;
let rm_first = Unicode.remove_first;
let rm_edge = (d: Direction.t, t) =>
  switch (d) {
  | Left => rm_last(t)
  | Right => rm_first(t)
  };
let split_nth = Unicode.split_nth;
let insert_nth = (idx, s, t) => Unicode.insert_nth(t, idx, s);
let to_list = Unicode.to_list;
let of_list = Unicode.of_list;

/* Token Recognition Predicates */

/* A. Secondary Notation (Comments, Whitespace, etc.)  */
let empty = ""; /* This is invalid for view */
let space = " ";
let linebreak = "\n";
let comment_regexp = regexp("^#[^#\n]*#$"); /* Multiline comments not supported */
let is_comment = t => match(comment_regexp, t) || t == "#";
let is_comment_delim = t => t == "#";
let is_secondary = t => List.mem(t, [space, linebreak]) || is_comment(t);

/* STRINGS: special-case syntax */

/* is_string: last clause is a somewhat hacky way of making sure
   there are at most two quotes, in order to prevent merges */
let string_regexp = regexp("^\"[^\n]*\"$"); /* Multiline strings not supported */
let is_string = t =>
  match(string_regexp, t) && List.length(split_on_char('"', t)) < 4;
let string_delim = "\"";
let empty_string = append(string_delim, string_delim);
let is_string_delim = (==)(string_delim);
/* Byte-based, which is exactly right here: every delimiter we quote with is
   a one-byte ASCII character, so the bytes stripped are the delimiters
   whatever the quoted content is. */
let strip_quotes = (~quote="\"", s) =>
  if (String.length(s) < 2) {
    s;
  } else if (String.sub(s, 0, 1) != quote
             || String.sub(s, String.length(s) - 1, 1) != quote) {
    s;
  } else {
    String.sub(s, 1, String.length(s) - 2);
  };

let string_quote = s => "\"" ++ s ++ "\"";

/* Grapheme width: functions taking into account that some unicode
   clusters (emoji, CJK, fullwidth forms) occupy two grid columns.
   These apply to EVERY token; Unicode.Width short-circuits on ASCII, so
   the common case costs a byte scan. */

let column_to_grapheme_index = Unicode.Width.column_to_grapheme_index;

/* Measured columns occupied by the first `count` graphemes of the token. */
let prefix_columns = (t: t, count: int): int =>
  Unicode.Width.columns_through_prefix(t, count);

let columns = Unicode.Width.columns_of_string;

let bounding_box = (t: t): Point.t => {
  let (row, col) = Unicode.Width.bounding_box_for(t);
  Point.mk(~row, ~col);
};

let quoted_label_regexp = regexp("^`[^`\n]*`$");
let is_quoted_label = t => match(quoted_label_regexp, t);
let label_delim = "`";
let is_quoted_label_delim = (==)(label_delim);
let label_quote = s => label_delim ++ s ++ label_delim;

let closing_stringlit_or_comment = (char, t: t): bool =>
  is_string(t)
  && is_string_delim(char)
  || is_comment(t)
  && is_comment_delim(char)
  || is_quoted_label(t)
  && is_quoted_label_delim(char);

let is_string_or_comment = t =>
  is_string(t) || is_comment(t) || is_quoted_label(t);
let is_string_or_comment_delim = t =>
  is_string_delim(t) || is_comment_delim(t) || is_quoted_label_delim(t);

let bools = ["true", "false"];
let is_bool = match(regexp("^(" ++ concat("|", bools) ++ ")$"));
let undefined = "undefined";
let is_undefined = match(regexp("^" ++ undefined ++ "$"));
let wild = "_";
let is_wild = match(regexp("^" ++ wild ++ "$"));

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
  "module",
];

let is_keyword = match(regexp("^(" ++ concat("|", keywords) ++ ")$"));

/* Potential tokens: These are fallthrough classes which determine
 * the behavior when inserting a character in contact with a token */

/* Operators are a closed list; names are everything left over. That keeps
 * the two disjoint by construction (mold resolution needs it) and makes a
 * new Unicode character default to being a name.
 *
 * The ASCII 18 are a pin: exactly the operator characters from before names
 * took Unicode. A character earns a place in the Unicode half by appearing
 * in operator POSITION, not by being printed -- these five are the Drv
 * judgment symbols ExpToSegment emits as infix tiles. */
let ascii_operator_chars = {|!%&*+,\-./:;<=>@\\|~|};
let unicode_operator_chars = {|∈≠≮≯⊆|};
let operator_chars = ascii_operator_chars ++ unicode_operator_chars;

/* Neither class: delimiters, whitespace, control characters, and the
 * implicit-hole marker. ¿ is excluded so a decoded slide like `[1, ¿, 3]`
 * doesn't merge `¿,` into one token; see Haz3lcore.MarkerParse. */
let excluded_chars = {|"`#¿\s\x00-\x1F\x7F\[\]\(\)\{\}|};

/* Names are the complement, so `é 日 😀 © ✓ λ` all behave alike and a
 * decomposed `é` stays one name. On ASCII this is exactly `a-zA-Z0-9_'?^$`
 * as before; the var/ctr regexps below drop all but `_` and `'`. */
let non_name_chars = operator_chars ++ excluded_chars;

/* UTF-8-aware rather than StringUtil.match: Js_of_ocaml.Regexp is
 * byte-oriented, so `¿` in a class excludes its two bytes independently --
 * rejecting `¢ £ ± ¬` and anything ending 0xBF -- and `\s` rejects anything
 * containing byte 0xA0. */
let is_potential_operand =
  unicode_match(
    unicode_regexp(
      "^([^"
      ++ non_name_chars
      ++ "]+)$|^([0-9_]+\\.(?:[^"
      ++ non_name_chars
      ++ {|\^\$]|\.)*)$|},
    ),
  );

let is_potential_operator =
  /* Multiline operators not supported */
  unicode_match(unicode_regexp("^[" ++ operator_chars ++ "]+$"));

/* `^` leads the livelit and projector-invocation prefixes, so a token
 * starting with it wants the same spacing an operator gets. */
let begins_with_potential_operator =
  unicode_match(unicode_regexp("^[" ++ operator_chars ++ {|\^]+|}));

let is_potential_token = t =>
  if (match(regexp("^>"), t)) {
    /* This case is necessary due to the ambiguity between operators
     * beginning with `>` and `>` as closing delimiter for type ap;.
     * e.g. `map@<a>==1 has an ambiguous lex otherwise*/
    t == ">" || t == ">=" || t == ">." || t == ">=.";
  } else {
    t == "()"
    || t == "[]"
    || t == "{}"
    || t == "¿"  /* implicit-hole marker; see Haz3lcore.MarkerParse */
    || is_potential_operand(t)
    || is_potential_operator(t)
    || is_string(t)
    || is_comment(t)
    || is_quoted_label(t);
  };

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

/* CASE. Hazel tells constructors from variables by capitalization, but most
 * of Unicode has no case at all. Caseless characters count as NON-uppercase,
 * so `日本語` and `😀foo` are variables while `Café` and `Foo😀` are
 * constructors (the rule Swift and Julia use). Titlecase letters (`ǅ`) count
 * as uppercase. */
let uppercase_chars = {|\p{Lu}\p{Lt}|};

/* What may continue a name: every name character except the ASCII
 * modifier-ish ones, which only ever prefix or suffix a token. `'` is
 * allowed in vars but not constructors, matching the ASCII behaviour. What
 * may START a name additionally excludes digits, so `1abc` is not a name. */
let name_start_class = "[^" ++ non_name_chars ++ {|0-9'?\^\$]|};
let name_rest_class = "[^" ++ non_name_chars ++ {|'?\^\$]|};
let var_rest_class = "[^" ++ non_name_chars ++ {|?\^\$]|};

let lowercase_start = "(?![" ++ uppercase_chars ++ "])" ++ name_start_class;
let uppercase_start = "[" ++ uppercase_chars ++ "]";

let is_livelit =
  unicode_match(
    unicode_regexp("^\\^" ++ lowercase_start ++ name_rest_class ++ "*$"),
  );

let parse_livelit = (str): string =>
  if (length(str) > 1 && starts_with(~prefix="^", str)) {
    rm_first(str);
  } else {
    "invalid form";
  };

/* Plain name; `$`-prefixed name (either case); module-qualified name. */
let var_regexp =
  unicode_regexp(
    "(^"
    ++ lowercase_start
    ++ var_rest_class
    ++ "*$)|(^\\$"
    ++ name_start_class
    ++ var_rest_class
    ++ "*$)|(^"
    ++ uppercase_start
    ++ var_rest_class
    ++ "*\\."
    ++ lowercase_start
    ++ var_rest_class
    ++ "*$)",
  );
let is_var = str =>
  !is_bool(str)
  && !is_undefined(str)
  && !is_livelit(str)
  && !is_wild(str)
  && unicode_match(var_regexp, str);

let capitalized_name_regexp =
  unicode_regexp("^" ++ uppercase_start ++ name_rest_class ++ "*$");
let is_ctr = unicode_match(capitalized_name_regexp);

let quote_label_when_necessary = (l: string): string =>
  is_var(l) || is_ctr(l) ? l : label_quote(l);
/* Atom type names recognized by MakeTerm as Atom(...) in Typ sort.
 * Also includes Drv* names recognized as DrvQuoteTy(sort).
 * Keep in sync with Ctx.is_base_typ. */
let base_typs = [
  "Bool",
  "Float",
  "Int",
  "Nat",
  "SInt",
  "String",
  "Void",
  "DrvJdmt",
  "DrvCtx",
  "DrvProp",
  "ALFAExp",
  "DrvPat",
  "ALFATyp",
  "DrvTPat",
];
let is_base_typ = match(regexp("^(" ++ concat("|", base_typs) ++ ")$"));
let is_typ_var = str =>
  is_var(str) || unicode_match(capitalized_name_regexp, str);

/* List literals */
let list_start = "[";
let list_end = "]";
let listlit_lbl = [list_start, list_end];
let empty_list = append(list_start, list_end);
let is_empty_list = equal(empty_list);

/* Tuples */
let tuple_start = "(";
let tuple_end = ")";
let tuple_lbl = [tuple_start, tuple_end];
let empty_tuple = append(tuple_start, tuple_end);
let is_empty_tuple = equal(empty_tuple);

/* Modules */
let mod_start = "{";
let mod_end = "}";
let mod_lbl = [mod_start, mod_end];
let empty_module = append(mod_start, mod_end);
let is_empty_module = equal(empty_module);

let const_mono_delims =
  base_typs
  @ bools
  @ [undefined, wild, empty_list, empty_tuple, empty_module, empty_string];

let bad_token_cls: string => bad_token_cls =
  t =>
    switch () {
    | _ when is_bad_int(t) => BadInt
    | _ => Other
    };

/* Explicit hole syntax */

let explicit_hole = "?";
let llm_hole = "??";
let llm_advanced_reasoning_hole = "?a";
let is_explicit_hole = t => t == explicit_hole;

/* Implicit-hole marker: the textual stand-in for a Grout piece used by
 * Haz3lcore.MarkerParse so decode|encode round-trips preserve Grout
 * positions. A single non-identifier, non-operator character that the
 * tokeniser treats as its own atomic token (won't glue with adjacent
 * commas, semicolons, or identifiers). */
let implicit_hole_marker = "¿";
let is_implicit_hole_marker = t => t == implicit_hole_marker;
let is_llm_hole = t => t == llm_hole || t == llm_advanced_reasoning_hole;

/* Projector invocation textual syntax */
let projector_invoke_prefix = "^^";

/* Strip the `^^` prefix, yielding the invoke body — option suffix and all,
   unlike of_projector_invoke_base below. No validation; that is
   is_projector_invoke's job.
     "^^probe_table" ==> Some("probe_table")   (base gives Some("probe"))
     "^^p"           ==> Some("p")   (no such kind; still stripped)
     "let" / "^^"    ==> None */
let of_projector_invoke = (input: t): option(t) =>
  if (starts_with(~prefix=projector_invoke_prefix, input)
      && length(input) > 2) {
    Some(snd(split_nth(input, 2)));
  } else {
    None;
  };

/* A `_opt` suffix on the invoke body is a trigger OPTION (e.g. the
   probe renderer in `^^probe_table`) — stripped for validity; Triggers
   parses the option itself. `_` is safe: no kind name contains one,
   and it merges into a single editor token. Splits on the FIRST `_`;
   whatever follows is the option verbatim.
     "probe"       ==> ("probe", None)
     "probe_table" ==> ("probe", Some("table"))
     "probe_a_b"   ==> ("probe", Some("a_b")) */
let split_invoke_opt = (body: t): (t, option(t)) =>
  switch (StringUtil.split_first(~on='_', body)) {
  | Some((base, opt)) => (base, Some(opt))
  | None => (body, None)
  };

/* The option a trigger token selects, if it carries one.
   "^^probe_table" ==> Some("table")
   "^^probe"       ==> None
   "let"           ==> None   (not a trigger at all) */
let of_projector_invoke_opt = (input: t): option(t) =>
  Option.bind(of_projector_invoke(input), body =>
    snd(split_invoke_opt(body))
  );

/* The kind name a trigger token names, with any option stripped.
   "^^probe_table" ==> Some("probe")
   "^^probe"       ==> Some("probe")
   "let"           ==> None   (not a trigger at all) */
let of_projector_invoke_base = (input: t): option(t) =>
  Option.map(
    body => fst(split_invoke_opt(body)),
    of_projector_invoke(input),
  );

/* Does this token name a known projector kind? Checks the WHOLE body, so a
   trigger carrying an option fails here even though its base names a kind
   — Triggers.is_refractor_trigger is the option-aware counterpart, which
   is why Triggers.expand_projector tries the refractor arm first.
     "^^probe" / "^^slider" ==> true
     "^^probe_table"        ==> false  (no kind is named "probe_table")
     "^^p" / "let" / "^^"   ==> false */
let is_projector_invoke = (str: t): bool =>
  switch (of_projector_invoke(str)) {
  | Some(name) => ProjectorCore.Kind.is_name(name)
  | None => false
  };

/* The trigger token naming a kind. Never carries an option suffix; callers
   that want one (Triggers.refractor_to_invoke) append `_opt` themselves.
     Probe ==> "^^probe" */
let mk_projector_invoke = (kind: ProjectorCore.Kind.t): string =>
  append(projector_invoke_prefix, ProjectorCore.Kind.name(kind));

/* Unicode probe brackets for CLI text output */
let probe_start = "⟦";
let probe_end = "⟧";
