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

/* a name typed from [text], its caret at the end */
let mk = (~row=?, ~anchor=?, ~inside=false, text: string): t => {
  ed_row: row,
  ed_anchor: anchor,
  ed_inside: inside,
  ed_text: text,
  ed_caret: String.length(text),
  ed_error: None,
};

/* structural operations on a definition, from its row's context menu;
   ItemEdit applies them at the item's owning block */
[@deriving (show({with_path: false}), sexp, yojson)]
type def_op =
  | NewBelow
  | NewTypeBelow
  | NewModuleBelow
  | Duplicate
  | MoveUp
  | MoveDown
  | Delete;

/* a new definition's kind from its leading keyword */
let new_kind = (text: string): (OutlineTree.kind, string) =>
  String.starts_with(~prefix="type ", text)
    ? (KType, "type ")
    : String.starts_with(~prefix="module ", text)
        ? (KModule, "module ") : (KConst, "");
