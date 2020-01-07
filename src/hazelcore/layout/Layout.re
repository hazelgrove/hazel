open Sexplib.Std;

/* Variable: `layout` */
[@deriving sexp]
type t('tag) = {
  layout: layout('tag),
  first_width: int,
  width: int,
  last_width: int,
  cost: int,
}
and layout('tag) =
  | Text(string) // Invariant: contains no newlines. Text("") is identity for `Cat`
  | Cat(t('tag), t('tag)) // associative // TODO: list
  | Linebreak
  | Align(t('tag))
  | Tagged('tag, t('tag)); // TODO: annot

let t_of_layout = (layout: layout('tag)): t('tag) => {
  layout,
  first_width: (-1),
  width: (-1),
  last_width: (-1),
  cost: (-1),
};

let rec remove_tags = (layout: t('tag)): t('tag) => {
  switch (layout.layout) {
  | Tagged(_, l) => remove_tags(l)
  | _ =>
    let l' =
      switch (layout.layout) {
      | Text(string) => Text(string)
      | Cat(l1, l2) => Cat(remove_tags(l1), remove_tags(l2))
      | Linebreak => Linebreak
      | Align(l) => Align(remove_tags(l))
      | Tagged(_, _) => failwith(__LOC__)
      };
    {...layout, layout: l'};
  };
};

// TODO?
//let text = (string) => t_of_layout(Text(string));
//let cat = (t1, t2) => t_of_layout(Cat(t1, t2));
//let linebreak = t_of_layout(linebreak);

// TODO: move to own module
type text('tag, 'imp, 't) = {
  // TODO: rename `imp`
  imp_of_string: string => 'imp,
  imp_append: ('imp, 'imp) => 'imp,
  imp_newline: int => 'imp,
  imp_of_tag: ('tag, 'imp) => 'imp,
  t_of_imp: 'imp => 't,
};

let make_of_layout: (text('tag, 'imp, 't), t('tag)) => 't =
  (text, layout) => {
    let column: ref(int) = ref(0);
    let rec go: (int, t('tag)) => 'imp =
      (indent, layout) => {
        switch (layout.layout) {
        | Text(string) =>
          column := column^ + CamomileLibrary.UTF8.length(string);
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
        | Tagged(tag, l) => text.imp_of_tag(tag, go(indent, l))
        };
      };
    text.t_of_imp(go(0, layout));
  };

let string_of_layout: 'tag. t('tag) => string =
  layout => {
    let record: 'tag. text('tag, string, string) = {
      imp_of_string: string => string,
      imp_append: (s1, s2) => s1 ++ s2,
      imp_newline: indent => "\n" ++ String.make(indent, ' '),
      imp_of_tag: (_, imp) => imp,
      t_of_imp: imp => imp,
    };
    make_of_layout(record, layout);
  };

let strings_of_layout: 'tag. t('tag) => list((int, string)) =
  layout => {
    let record: 'tag. text('tag, list((int, string)), list((int, string))) = {
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
      imp_of_tag: (_, imp) => imp,
      t_of_imp: s => s,
    };
    make_of_layout(record, layout);
  };
