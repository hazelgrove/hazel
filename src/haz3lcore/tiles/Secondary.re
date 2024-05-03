open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type cls =
  | Whitespace
  | Comment;

[@deriving (show({with_path: false}), sexp, yojson)]
type secondary_content =
  | Whitespace(string)
  | Comment(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  content: secondary_content,
};

let cls_of = (s: t): cls =>
  switch (s.content) {
  | Whitespace(_) => Whitespace
  | Comment(_) => Comment
  };

let mk_space = id => {content: Whitespace(Form.space), id};

let mk_newline = id => {content: Whitespace("\n"), id};

let construct_comment = content =>
  if (String.equal(content, "#")) {
    Comment("##");
  } else {
    Comment(content);
  };

let is_space: t => bool =
  w =>
    switch (w.content) {
    | Whitespace(s) => s == Form.space
    | _ => false
    };

let is_linebreak: t => bool =
  w =>
    switch (w.content) {
    | Whitespace(s) => s == Form.linebreak
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
