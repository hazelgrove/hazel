open Sexplib.Std;
open GeneralUtil;

/* Variable: `layout` */
[@deriving sexp]
type t('tag) = {
  metrics,
  layout: layout('tag),
}
and metrics = {
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

let align = (l: t('tag)) => Align(l);
let tag = (tag: 'tag, l: t('tag)) => Tagged(tag, l);

let mk_Tagged = (metrics, tag, l) => {metrics, layout: Tagged(tag, l)};

let t_of_layout = (layout: layout('tag)): t('tag) => {
  layout,
  metrics: {
    first_width: (-1),
    width: (-1),
    last_width: (-1),
    cost: (-1),
  },
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
          column := column^ + GeneralUtil.utf8_length(string);
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

let rec query_next_Tagged =
        (query: (metrics, 'tag, t('tag)) => option('a), l: t('tag))
        : option('a) => {
  let go = query_next_Tagged(query);
  switch (l.layout) {
  | Linebreak
  | Text(_) => None
  | Align(l1) => go(l1)
  | Cat(l1, l2) =>
    switch (go(l1)) {
    | Some(_) as found => found
    | None => go(l2)
    }
  | Tagged(tag, l1) => query(l.metrics, tag, l1)
  };
};
