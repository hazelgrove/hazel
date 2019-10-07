open Sexplib.Std;

/* Variable: `layout` */
[@deriving sexp]
type t('tag) =
  | Empty /* identity for VCat and HCat */
  | VCat(t('tag), t('tag)) /*associative*/
  | HCat(t('tag), t('tag)) // associative
  | Text(string) /* Text("") is identity for HCat if not Empty */
  | Align(t('tag))
  | Tagged('tag, t('tag));

let string_of_layout: t('tag) => string = {
  let rec go =
          (first_left: int, left: int)
          : (t('tag) => list((int /* indent */, string))) =>
    // Assert first_left >= left
    fun
    | Empty => []
    | VCat(l1, l2) => {
        switch (go(first_left, left, l1)) {
        | [] => go(first_left, left, l2)
        | s1 => s1 @ go(left, left, l2)
        };
      }
    | HCat(l1, l2) =>
      switch (GeneralUtil.split_last(go(first_left, left, l1))) {
      | None => go(first_left, left, l2)
      | Some((init, (indent, last))) =>
        switch (go(indent + String.length(last), left, l2)) {
        | [] => init @ [(indent, last)]
        | [(_, head), ...tail] => init @ [(indent, last ++ head), ...tail]
        }
      }
    | Text(string) => [(first_left, string)]
    | Align(l) => go(first_left, first_left, l)
    | Tagged(_tag, l) => go(first_left, left, l);
  let indent = ((i: int, string: string)): string =>
    String.make(i, ' ') ++ string;
  layout => {
    String.concat("\n", List.map(indent, go(0, 0, layout)));
  };
};
