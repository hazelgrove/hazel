open Sexplib.Std;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  content: secondary_content,
}
and secondary_content =
  | Whitespace(string)
  | Comment(string);

let space = " ";
let linebreak = "⏎"; //alternative: "¶"
let comment = Re.Str.regexp("^#[^#]*#$");

let mk_space = id => {content: Whitespace(space), id};

let construct_comment = content =>
  if (String.equal(content, "#")) {
    Comment("##");
  } else {
    Comment(content);
  };

let is_space: t => bool =
  w =>
    switch (w.content) {
    | Whitespace(s) => s == space
    | _ => false
    };

let is_linebreak: t => bool =
  w =>
    switch (w.content) {
    | Whitespace(s) => s == linebreak
    | _ => false
    };

let content_is_comment: secondary_content => bool =
  content =>
    switch (content) {
    | Comment(_) => true
    | _ => false
    };

let is_comment: t => bool =
  w =>
    switch (w.content) {
    | Comment(_) => true
    | _ => false
    };

// Returns the string value of the Whitespace
let get_string: secondary_content => string =
  content =>
    switch (content) {
    | Comment(s)
    | Whitespace(s) => s
    };

let id = w => w.id;
