open Util;

module WidthPosKey = {
  type t = (int, int);
  let hash = ((width, pos)) => 256 * 256 * width + pos;
  let equal = ((w1, p1), (w2, p2)) => w1 == w2 && p1 == p2;
};

module M = Hashtbl.Make(WidthPosKey);

// NOTE: pos is relative to most recent `Align`
type m'('a) = PosMap.t((Cost.t, 'a));
type m('a) = (~width: int, ~pos: int) => m'('a);

[@deriving sexp]
type t('annot) = {
  mem: [@sexp.opaque] M.t(m'(Layout.t('annot))),
  doc: t'('annot),
}
[@deriving sexp]
and t'('annot) =
  | Text(string) // Text("") is identity for `Cat`
  | Cat(t('annot), t('annot)) // associative
  | Linebreak
  | Align(t('annot))
  | Annot('annot, t('annot)) // Annotations
  | Fail // identity for `Choice`
  | Choice(t('annot), t('annot));

let t_of_t' = (t': t'('annot)): t('annot) => {mem: M.create(0), doc: t'};

let text = (s: string) => t_of_t'(Text(s));
let linebreak = () => t_of_t'(Linebreak);
let align = doc => t_of_t'(Align(doc));
let annot = (annot, doc) => t_of_t'(Annot(annot, doc));
let fail = () => t_of_t'(Fail);

let empty = () => text("");
let space = () => text(Unicode.nbsp); // TODO: param to hsep

let indent = () => text(Unicode.nbsp ++ Unicode.nbsp);
let indent_and_align = doc => t_of_t'(Cat(indent(), align(doc)));

let hcat = (x, y) => t_of_t'(Cat(x, y));
let hcats: list(t('annot)) => t('annot) =
  fun
  | [] => empty()
  | [doc, ...docs] => List.fold_left(hcat, doc, docs);

let hsep = (x, y) => t_of_t'(Cat(x, t_of_t'(Cat(space(), y))));
let hseps: list(t('annot)) => t('annot) =
  fun
  | [] => empty()
  | [doc, ...docs] => List.fold_left(hsep, doc, docs);

let vsep = (x, y) => t_of_t'(Cat(x, t_of_t'(Cat(linebreak(), y))));
let vseps: list(t('annot)) => t('annot) =
  fun
  | [] => failwith(__LOC__ ++ ": vseps requires a non-empty list")
  | [doc] => doc
  | [doc, ...docs] => List.fold_left(vsep, doc, docs);

let choice = (x, y) => t_of_t'(Choice(x, y));
let choices: list(t('annot)) => t('annot) =
  fun
  | [] => fail()
  | [doc, ...docs] => List.fold_left(choice, doc, docs);

let map_t': 'a 'b. (t'('a) => t'('b), t('a)) => t('b) =
  (f, d) => t_of_t'(f(d.doc));

let rec map_annot: 'a 'b. ('a => 'b, t('a)) => t('b) =
  (f, d) =>
    d
    |> map_t'(
         fun
         | (Text(_) | Linebreak | Fail) as d' => d'
         | Annot(annot, d) => Annot(f(annot), map_annot(f, d))
         | Align(d) => Align(map_annot(f, d))
         | Cat(d1, d2) => Cat(map_annot(f, d1), map_annot(f, d2))
         | Choice(d1, d2) => Choice(map_annot(f, d1), map_annot(f, d2)),
       );
