open Util;

// make an enum
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = string;

[@deriving (show({with_path: false}), sexp, yojson)]
type bad_token_cls =
  | Other
  | BadInt;

let length = Unicode.length;
let compare = String.compare;
let equal = String.equal;
let sub = String.sub;
let concat = String.concat;
let starts_with = String.starts_with;
let split_on_char = String.split_on_char;
let append = (++);
let sort_uniq = List.sort_uniq(compare);
let rm_nth = StringUtil.remove_nth;
let rm_last = StringUtil.remove_last;
let rm_first = StringUtil.remove_first;
let rm_edge = (d: Direction.t, t) =>
  switch (d) {
  | Left => rm_last(t)
  | Right => rm_first(t)
  };
let split_nth = StringUtil.split_nth;
let insert_nth = StringUtil.insert_nth;
let match = StringUtil.match;
let regexp = StringUtil.regexp;
let prefixes = StringUtil.prefixes;
let to_list = StringUtil.to_list;
let abbreviate = StringUtil.abbreviate;
let num_linebreaks = StringUtil.num_linebreaks;
let max_line_width = StringUtil.max_line_width;

let bounding_box = (t: t): Point.t =>
  Point.mk(~row=num_linebreaks(t), ~col=max_line_width(t));

/* Token Recognition Predicates */

/* A. Secondary Notation (Comments, Whitespace, etc.)  */
let empty = "";
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
];

let is_keyword = match(regexp("^(" ++ concat("|", keywords) ++ ")$"));

/* Potential tokens: These are fallthrough classes which determine
 * the behavior when inserting a character in contact with a token */

let is_potential_operand =
  match(regexp("^([a-zA-Z0-9_'?\\^]+)$|^([0-9_]+\\.[a-zA-Z0-9_'\\.?]*)$"));
/* Anything else is considered a potential operator, as long
 *  as it does not contain any whitespace, linebreaks, comment
 *  delimiters, string delimiters, or the instant expanding paired
 *  delimiters: ()[]| */

let is_potential_operator =
  /* Multiline operators not supported */
  match(regexp("^[^a-zA-Z0-9_'?\\^\"`#\n\\s\\[\\]\\(\\)]+$"));

let begins_with_potential_operator =
  match(regexp("^[^a-zA-Z0-9_'?\"`#\n\\s\\[\\]\\(\\)]+"));

let is_potential_token = t =>
  if (match(regexp("^>"), t)) {
    /* This case is necessary due to the ambiguity between operators
     * beginning with `>` and `>` as closing delimiter for type ap;.
     * e.g. `map@<a>==1 has an ambiguous lex otherwise*/
    t == ">" || t == ">=" || t == ">." || t == ">=.";
  } else {
    t == "()"
    || t == "[]"
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
let is_bad_float = str => is_arbitary_float(str) && !is_float(str);

let is_livelit = str => match(regexp("^(\\^)([a-z][A-Za-z0-9_]*)$"), str);

let parse_livelit = (str): string =>
  if (length(str) > 1 && sub(str, 0, 1) == "^") {
    sub(str, 1, length(str) - 1);
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
  && !is_wild(str)
  && match(var_regexp, str);

let quote_label_when_necessary = (l: string): string =>
  is_var(l) ? l : label_quote(l);

let capitalized_name_regexp = regexp("^[A-Z][A-Za-z0-9_]*$");
let is_ctr = match(capitalized_name_regexp);
let base_typs = ["String", "Int", "Float", "Bool"];
let is_base_typ = match(regexp("^(" ++ concat("|", base_typs) ++ ")$"));
let is_typ_var = str => is_var(str) || match(capitalized_name_regexp, str);

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

let const_mono_delims =
  base_typs @ bools @ [undefined, wild, empty_list, empty_tuple, empty_string];

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
let is_llm_hole = t => t == llm_hole || t == llm_advanced_reasoning_hole;

/* Projector invocation textual syntax */
let projector_invoke_prefix = "^^";

let of_projector_invoke = (input: t): option(t) =>
  if (starts_with(~prefix=projector_invoke_prefix, input)
      && length(input) > 2) {
    Some(sub(input, 2, length(input) - 2));
  } else {
    None;
  };

let is_projector_invoke = (str: t): bool =>
  switch (of_projector_invoke(str)) {
  | Some(name) => ProjectorCore.Kind.is_name(name)
  | None => false
  };

let mk_projector_invoke = (kind: ProjectorCore.Kind.t): string =>
  append(projector_invoke_prefix, ProjectorCore.Kind.name(kind));
