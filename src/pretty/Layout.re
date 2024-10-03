open Util;

// TODO: rename Annot to Ann?
[@deriving sexp]
type t('annot) =
  | Text(string) // Invariant: contains no newlines. Text("") is identity for `Cat`
  | Cat(t('annot), t('annot)) // Associative
  | Linebreak
  | Align(t('annot))
  | Annot('annot, t('annot)); // Annotations

let align = (l: t('annot)) => Align(l);
let annot = (annot: 'annot, l: t('annot)) => Annot(annot, l);

let rec remove_annots = (layout: t('annot)): t('annot) => {
  switch (layout) {
  | Annot(_, l) => remove_annots(l)
  | Text(string) => Text(string)
  | Cat(l1, l2) => Cat(remove_annots(l1), remove_annots(l2))
  | Linebreak => Linebreak
  | Align(l) => Align(remove_annots(l))
  };
};

// TODO?
//let text = (string) => t_of_layout(Text(string));
//let cat = (t1, t2) => t_of_layout(Cat(t1, t2));
//let linebreak = t_of_layout(linebreak);

// TODO: move to own module
type text('annot, 'imp, 't) = {
  // TODO: rename `imp`
  imp_of_string: string => 'imp,
  imp_append: ('imp, 'imp) => 'imp,
  imp_newline: int => 'imp,
  imp_of_annot: ('annot, 'imp) => 'imp,
  t_of_imp: 'imp => 't,
};

let mk_of_layout: (text('annot, 'imp, 't), t('annot)) => 't =
  (text, layout) => {
    let column: ref(int) = ref(0);
    let rec go: (int, t('annot)) => 'imp =
      (indent, layout) => {
        switch (layout) {
        | Text(string) =>
          column := column^ + Unicode.length(string);
          text.imp_of_string(string);
        | Cat(l1, l2) =>
          let imp1 = go(indent, l1);
          let imp2 = go(indent, l2);
          text.imp_append(imp1, imp2);
        | Linebreak =>
          // TODO: no indent if on final line break
          column := indent;
          text.imp_newline(indent);
        | Align(l) => go(column^, l)
        | Annot(annot, l) => text.imp_of_annot(annot, go(indent, l))
        };
      };
    text.t_of_imp(go(0, layout));
  };

let string_of_layout: 'annot. t('annot) => string =
  layout => {
    let record: 'annot. text('annot, string, string) = {
      imp_of_string: string => string,
      imp_append: (s1, s2) => s1 ++ s2,
      imp_newline: indent => "\n" ++ String.make(indent, ' '),
      imp_of_annot: (_, imp) => imp,
      t_of_imp: imp => imp,
    };
    mk_of_layout(record, layout);
  };

let strings_of_layout: 'annot. t('annot) => list((int, string)) =
  layout => {
    let record:
      'annot.
      text('annot, list((int, string)), list((int, string)))
     = {
      imp_of_string: string => [(0, string)],
      imp_append: (s1, s2) => {
        switch (List.rev(s1), s2) {
        | ([], _) => s2
        | (_, []) => s1
        | (
            [(last_indent_1, last_string_1), ...rest1],
            [(first_indent_2, first_string_2), ...rest2],
          ) =>
          assert(first_indent_2 == 0);
          List.rev(rest1)
          @ [(last_indent_1, last_string_1 ++ first_string_2), ...rest2];
        };
      },
      imp_newline: indent => [(indent, "")],
      imp_of_annot: (_, imp) => imp,
      t_of_imp: s => s,
    };
    mk_of_layout(record, layout);
  };
