open Sexplib.Std;

// type t('tag) = {
//   metrics,
//   layout: layout('tag),
// }
// and metrics = {
//   first_width: int,
//   width: int,
//   last_width: int,
//   cost: int,
// }

[@deriving sexp]
type t('tag) =
  | Text(string) // Invariant: contains no newlines. Text("") is identity for `Cat`
  | Cat(t('tag), t('tag)) // associative // TODO: list
  | Linebreak
  | Align(t('tag))
  | Tagged('tag, t('tag)); // TODO: annot

let align = (l: t('tag)) => Align(l);
let tag = (tag: 'tag, l: t('tag)) => Tagged(tag, l);

type metrics = {
  height: int,
  last_width: int,
  last_width_is_relative: bool,
};

let rec metrics: 'tag. t('tag) => metrics =
  // TODO: rename
  layout => {
    Obj.magic(Lazy.force(metrics_memo_table, Obj.magic(layout)));
  }

and metrics_memo_table: Lazy.t(t(unit) => metrics) =
  lazy(Memoize.WeakPoly.make(metrics'))

and metrics' = (layout: t(unit)): metrics =>
  switch (layout) {
  | Text(string) => {
      height: 1,
      last_width: String.length(string),
      last_width_is_relative: true,
    }
  | Linebreak => {height: 2, last_width: 0, last_width_is_relative: false}
  | Tagged(_, l) => metrics(l)
  | Align(l) => {...metrics(l), last_width_is_relative: false}
  | Cat(l1, l2) =>
    let metrics1 = metrics(l1);
    let metrics2 = metrics(l2);
    let height = metrics1.height + metrics2.height - 1;
    let metrics =
      if (!metrics2.last_width_is_relative) {
        metrics2;
      } else {
        {...metrics1, last_width: metrics1.last_width + metrics2.last_width};
      };
    {...metrics, height};
  };

let rec remove_tags = (layout: t('tag)): t('tag) => {
  switch (layout) {
  | Tagged(_, l) => remove_tags(l)
  | Text(string) => Text(string)
  | Cat(l1, l2) => Cat(remove_tags(l1), remove_tags(l2))
  | Linebreak => Linebreak
  | Align(l) => Align(remove_tags(l))
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
        switch (layout) {
        | Text(string) =>
          column := column^ + StringUtil.utf8_length(string);
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

/* TODO got weird type inference error, see specialized instance in TermLayout
   let rec find_and_decorate_Tagged =
           (decorate: ('tag, t('tag)) => decorate_result('tag), l: t('tag))
           : option(t('tag)) => {
     let go = find_and_decorate_Tagged(decorate);
     switch (l) {
     | Linebreak
     | Text(_) => None
     | Align(l) => l |> go |> OptUtil.map(align)
     | Cat(l1, l2) =>
       switch (l1 |> go) {
       | Some(l1) => Some(Cat(l1, l2))
       | None => l2 |> go |> OptUtil.map(l2 => Cat(l1, l2))
       }
     | Tagged(tg, l) =>
       switch (decorate(tag, l)) {
       | Failed => None
       | Skipped => l |> go |> OptUtil.map(tag(tg))
       | Decorated(l) => Some(l)
       }
     };
   };
   */
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
