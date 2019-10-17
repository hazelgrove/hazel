open Sexplib.Std;

/* Variable: `doc` */
[@deriving sexp]
type t('tag) =
  | Text(string) // Text("") is identity for `Cat`
  | Cat(t('tag), t('tag)) // associative
  | Linebreak
  | Align(t('tag))
  | Tagged('tag, t('tag))
  | Fail // identity for `Choice`
  | Choice(t('tag), t('tag));

let empty = Text("");
let space = Text(" ");

let hcat = (x, y) => Cat(x, y);
let hcats: list(t('tag)) => t('tag) =
  fun
  | [] => empty
  | [doc, ...docs] => List.fold_left(hcat, doc, docs);

let hsep = (x, y) => Cat(x, Cat(space, y));
let hseps: list(t('tag)) => t('tag) =
  fun
  | [] => empty
  | [doc, ...docs] => List.fold_left(hsep, doc, docs);

let vsep = (x, y) => Cat(x, Cat(Linebreak, y));
let vseps: list(t('tag)) => t('tag) =
  fun
  | [] => failwith(__LOC__ ++ ": vcats requires a non-empty list")
  | [doc] => doc
  | [doc, ...docs] => List.fold_left(vsep, doc, docs);

let choice = (x, y) => Choice(x, y);
let choices: list(t('tag)) => t('tag) =
  fun
  | [] => Fail
  | [doc, ...docs] => List.fold_left(choice, doc, docs);
