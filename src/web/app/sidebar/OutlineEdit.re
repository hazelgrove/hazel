open Util;

/* a name being typed in the outline: an existing row's (a rename), or a
   new definition's below [ed_anchor], or last inside it when
   [ed_inside] (a module) */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  ed_row: option(Language.Id.t),
  ed_anchor: option(Language.Id.t),
  ed_inside: bool,
  ed_text: string,
  ed_caret: int,
  ed_error: option(string),
};
