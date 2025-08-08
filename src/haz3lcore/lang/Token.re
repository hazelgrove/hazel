open Util;
open StringUtil;

// make an enum
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = string;

[@deriving (show({with_path: false}), sexp, yojson)]
type bad_token_cls =
  | Other
  | BadInt;

module Index = {
  type t = int;
};

let length = Unicode.length;
let compare = String.compare;
let rm_nth = Util.StringUtil.remove_nth;
let rm_last = Util.StringUtil.remove_last;
let rm_first = Util.StringUtil.remove_first;
let split_nth = Util.StringUtil.split_nth;
let insert_nth = Util.StringUtil.insert_nth;
let equal = String.equal;

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
  if (match(regexp("@"), t) && !(t == "@" || t == "@<")) {
    false; /* the expression `map@<a>@<a>` has an ambiguous lex otherwise*/
         //TODO(andrew): document
  } else {
    is_potential_operand(t)
    || is_potential_operator(t)
    || is_string(t)
    || is_comment(t);
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

let projector_invoke_prefix = "^^";

let of_projector_invoke = (input: string): option(string) =>
  if (String.starts_with(~prefix=projector_invoke_prefix, input)
      && String.length(input) > 2) {
    Some(String.sub(input, 2, String.length(input) - 2));
  } else {
    None;
  };

let is_projector_invoke = (str: string): bool =>
  switch (of_projector_invoke(str)) {
  | Some(name) => ProjectorCore.Kind.is_name(name)
  | None => false
  };

let mk_projector_invoke = (kind: ProjectorCore.Kind.t): string =>
  projector_invoke_prefix ++ ProjectorCore.Kind.name(kind);

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

let const_mono_delims =
  base_typs @ bools @ [undefined, wild, empty_list, empty_tuple, empty_string];

let explicit_hole = "?";
let llm_hole = "??";
let llm_advanced_reasoning_hole = "?a";
let is_explicit_hole = t => t == explicit_hole;
let is_llm_hole = t => t == llm_hole || t == llm_advanced_reasoning_hole;

let closing_stringlit_or_comment = (char, t) =>
  is_string(t)
  && is_string_delim(char)
  || is_comment(t)
  && is_comment_delim(char);

let bad_token_cls: string => bad_token_cls =
  t =>
    switch () {
    | _ when is_bad_int(t) => BadInt
    | _ => Other
    };

let allow_append_right = (t: t, char: string): bool =>
  is_potential_token(t ++ char);

let allow_append_left = (char: string, t: t): bool =>
  is_potential_token(t ++ char);

let allow_insertion = (_char: string, _t: t, new_t: t): bool =>
  is_potential_token(new_t);

let allow_merge = (l: t, r: t): bool => is_potential_token(l ++ r);

/* These functions determine which forms can switch back and forth between
   mono and duotile forms, like list literals and tuples switching to/from
   the empty list and empty tuple. Technically this should be derivable from
   the language data; leaving that for a future refactor. */
let duosplits = (t: t): list(t) =>
  switch () {
  | _ when is_empty_list(t) => listlit_lbl
  | _ when is_empty_tuple(t) => tuple_lbl
  | _ => []
  };

let duomerges = (lbl: list(t)): option(list(t)) =>
  switch () {
  | _ when lbl == listlit_lbl => Some([empty_list])
  | _ when lbl == tuple_lbl => Some([empty_tuple])
  | _ => None
  };
