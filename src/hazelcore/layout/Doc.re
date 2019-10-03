open Sexplib.Std;

/* Variable: `doc` */
[@deriving sexp]
type t('tag) =
  | VZero /* identity for VCat */
  | VCat(t('tag), t('tag))
  | HCat(t('tag), t('tag))
  | String(string)
  | Align(t('tag))
  | Tagged('tag, t('tag))
  | Choice(t('tag), t('tag));

/* identity for HCat */
let hzero = String("");

let hcats: list(t('tag)) => t('tag) =
  fun
  | [] => hzero
  | [doc, ...docs] =>
    docs |> List.fold_left((acc, d) => HCat(acc, d), doc);

let vcats: list(t('tag)) => t('tag) =
  fun
  | [] => VZero
  | [doc, ...docs] =>
    docs |> List.fold_left((acc, d) => VCat(acc, d), doc);

/* expects non-empty list */
let choices: list(t('tag)) => t('tag) =
  fun
  | [] => assert(false)
  | [doc, ...docs] =>
    docs |> List.fold_left((acc, d) => Choice(acc, d), doc);
