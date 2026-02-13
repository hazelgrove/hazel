open Util;
open Language.Secondary;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = Language.Secondary.t;

let mk_empty = id => {
  content: Whitespace(Token.empty),
  id,
};

let mk_space = id => {
  content: Whitespace(Token.space),
  id,
};

let mk_newline = id => {
  content: Whitespace(Token.linebreak),
  id,
};

let construct_comment = content =>
  if (String.equal(content, "#")) {
    Comment("##");
  } else {
    Comment(content);
  };

let mk = (id: Id.t, content: string): t => {
  id,
  content:
    Token.is_comment(content)
      ? construct_comment(content) : Whitespace(content),
};

let is_space: t => bool =
  w =>
    switch (w.content) {
    | Whitespace(s) => s == Token.space
    | _ => false
    };

let is_linebreak: t => bool =
  w =>
    switch (w.content) {
    | Whitespace(s) => s == Token.linebreak
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

let length = (s: t): int => Token.length(get_string(s.content));

let id = w => w.id;
