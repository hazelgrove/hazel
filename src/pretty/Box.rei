[@deriving sexp]
type t('annot) =
  | Text(string)
  | HBox(list(t('annot)))
  | VBox(list(t('annot)))
  | Annot('annot, t('annot));

module Make: (MemoTbl.S) => {let mk: Layout.t('annot) => t('annot);};
