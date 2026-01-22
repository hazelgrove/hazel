open Util;
open Language.Secondary;

[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type cls = Language.Secondary.cls;

/* Use the types from Language.Secondary directly for type compatibility with IdTagged */
type secondary_content = Language.Secondary.secondary_content;
type t = Language.Secondary.t;

/* Re-export derived functions from Language.Secondary */
let pp_secondary_content = Language.Secondary.pp_secondary_content;
let show_secondary_content = Language.Secondary.show_secondary_content;
let secondary_content_of_sexp = Language.Secondary.secondary_content_of_sexp;
let sexp_of_secondary_content = Language.Secondary.sexp_of_secondary_content;
let secondary_content_of_yojson = Language.Secondary.secondary_content_of_yojson;
let yojson_of_secondary_content = Language.Secondary.yojson_of_secondary_content;
let equal_secondary_content = Language.Secondary.equal_secondary_content;
let pp = Language.Secondary.pp;
let show = Language.Secondary.show;
let t_of_sexp = Language.Secondary.t_of_sexp;
let sexp_of_t = Language.Secondary.sexp_of_t;
let t_of_yojson = Language.Secondary.t_of_yojson;
let yojson_of_t = Language.Secondary.yojson_of_t;
let equal_t = Language.Secondary.equal;

let equal = (a: t, b: t) => a.content == b.content;
let cls_of = (s: t): cls =>
  switch (s.content) {
  | Whitespace(_) => Whitespace
  | Comment(_) => Comment
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
