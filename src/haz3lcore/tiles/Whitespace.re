open Sexplib.Std;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  content: whitespace_content,
}
and whitespace_content =
  | WSpace(string)
  | Comment(string);

//TODO: keep the Form.re whitespace and use a custom constructor to turn whitespace_content into strings.

let space = " ";
let linebreak = "⏎"; //alternative: "¶"
let comment = Re.Str.regexp("^#[^#]*#?$"); //added (?)
// "##"
let incomplete_comment1 = Re.Str.regexp("^#[_a-zA-Z0-9 ']*$"); // added
let incomplete_comment2 = Re.Str.regexp("^[_a-zA-Z0-9 ']*#$"); // added

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

let get_content_string: t => string =
  //TODO: remove this and only use get_string (rename that to get_content_string)
  w =>
    switch (w.content) {
    | Comment(s)
    | WSpace(s) => s
    };

let get_string: whitespace_content => string =
  content =>
    switch (content) {
    | Comment(s)
    | WSpace(s) => s
    };

// TODO: ADD string_to_whitespace

let id = w => w.id;
