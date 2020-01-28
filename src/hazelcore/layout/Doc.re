open Sexplib.Std;

/* Variable: `doc` */
[@deriving sexp]
type t('annot) =
  | Text(string) // Text("") is identity for `Cat`
  | Cat(t('annot), t('annot)) // associative
  | Linebreak
  | Align(t('annot))
  | Annot('annot, t('annot)) // Annotations
  | Fail // identity for `Choice`
  | Choice(t('annot), t('annot));

let empty = Text("");
let space = Text(UnicodeConstants.nbsp);
let indent = Text(UnicodeConstants.nbsp ++ UnicodeConstants.nbsp);

let align = doc => Align(doc);
let annot = (annot, doc) => Annot(annot, doc);

let hcat = (x, y) => Cat(x, y);
let hcats: list(t('annot)) => t('annot) =
  fun
  | [] => empty
  | [doc, ...docs] => List.fold_left(hcat, doc, docs);

let hsep = (x, y) => Cat(x, Cat(space, y));
let hseps: list(t('annot)) => t('annot) =
  fun
  | [] => empty
  | [doc, ...docs] => List.fold_left(hsep, doc, docs);

let vsep = (x, y) => Cat(x, Cat(Linebreak, y));
let vseps: list(t('annot)) => t('annot) =
  fun
  | [] => failwith(__LOC__ ++ ": vseps requires a non-empty list")
  | [doc] => doc
  | [doc, ...docs] => List.fold_left(vsep, doc, docs);

let choice = (x, y) => Choice(x, y);
let choices: list(t('annot)) => t('annot) =
  fun
  | [] => Fail
  | [doc, ...docs] => List.fold_left(choice, doc, docs);
