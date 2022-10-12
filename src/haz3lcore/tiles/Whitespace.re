open Sexplib.Std;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  content: whitespace_content,
}
and whitespace_content =
  | WSpace(string)
  | Comment(string);

let space = " ";
let linebreak = "⏎"; //alternative: "¶"
let comment = Re.Str.regexp("^#[^#]*#?$"); //added (?)
// "##"

let mk_space = id => {content: WSpace(space), id};

let is_space: t => bool =
  w =>
    switch (w.content) {
    | WSpace(s) => s == space
    | _ => false
    };

let is_linebreak: t => bool =
  w =>
    switch (w.content) {
    | WSpace(s) => s == linebreak
    | _ => false
    };

let is_comment: t => bool =
  w =>
    switch (w.content) {
    | Comment(_) => true
    | _ => false
    }; //added

let get_string: whitespace_content => string =
  content =>
    switch (content) {
    | Comment(s)
    | WSpace(s) => s
    };

let id = w => w.id;
