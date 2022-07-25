//TODO(andrew): autoformatter

[@deriving (show({with_path: false}), sexp, yojson)]
type padding =
  | None
  | Bi
  | Pre
  | Post;

let padding: string => padding =
  fun
  | "fun"
  | "let" => Post
  | "=>"
  | "+"
  | "-"
  | "*"
  | "/"
  | ","
  | "="
  | "in"
  | "?"
  | ":" => Bi
  | "("
  | ")"
  | "["
  | "]"
  | "}"
  | _ => None;

let format: Zipper.t => Zipper.t = z => z;

/*
 always: dont space-pad infix grout

 always: dont delete space immediately before caret; need it to type normally

 on format:
 regularize spaces around ops. default to the space/lack to the left
 (optional: space-pad or no-pad all ops)

 insert spaces around+between: fun, let, cond, case, rule

 then: get rid of double spaces, spaces at beginning/end of lines
 (again xcept near caret)


  */
