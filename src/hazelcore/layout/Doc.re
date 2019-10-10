open Sexplib.Std;

/* Variable: `doc` */
[@deriving sexp]
type t('tag) =
  | Empty // identity for VCat and HCat
  | Text(string) // Text("") is identity for HCat if the other t is not Empty
  | Align(t('tag))
  | Tagged('tag, t('tag))
  | VCat(t('tag), t('tag)) // associative
  | HCat(t('tag), t('tag)) // associative
  | Choice(t('tag), t('tag));

let hcats: list(t('tag)) => t('tag) =
  fun
  | [] => Empty
  | [doc, ...docs] =>
    docs |> List.fold_left((acc, d) => HCat(acc, d), doc);

let vcats: list(t('tag)) => t('tag) =
  fun
  | [] => Empty
  | [doc, ...docs] =>
    docs |> List.fold_left((acc, d) => VCat(acc, d), doc);

/* expects non-empty list */
let choices: list(t('tag)) => t('tag) =
  fun
  | [] => assert(false)
  | [doc, ...docs] =>
    docs |> List.fold_left((acc, d) => Choice(acc, d), doc);

let space = Text(" ");
let hspace = (doc1, doc2) => hcats([doc1, space, doc2]);
let hspaces = docs => hcats(GeneralUtil.join(space, docs));
