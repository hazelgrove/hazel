open Sexplib.Std;
open GeneralUtil;

/* Variable: `layout` */
[@deriving sexp]
type t('tag) =
  | Text(string) // Invariant: contains no newlines. Text("") is identity for `Cat`
  | Cat(t('tag), t('tag)) // associative // TODO: list
  | Linebreak
  | Align(t('tag))
  | Tagged('tag, t('tag)); // TODO: annot

let align = (l: t('tag)) => Align(l);
let tag = (tag: 'tag, l: t('tag)) => Tagged(tag, l);

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
      indent => {
        fun
        | Text(string) => {
            column := column^ + GeneralUtil.utf8_length(string);
            text.imp_of_string(string);
          }
        | Cat(l1, l2) => {
            let imp1 = go(indent, l1);
            let imp2 = go(indent, l2);
            text.imp_append(imp1, imp2);
          }
        | Linebreak => {
            // TODO: no indent if on final line break
            column := indent;
            text.imp_newline(indent);
          }
        | Align(l) => {
            go(column^, l);
          }
        | Tagged(tag, l) => text.imp_of_tag(tag, go(indent, l));
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

type decorate_result('tag) =
  | Failed
  | Skipped
  | Decorated(t('tag));

/* TODO got weird type inference error, see specialized instance in TermLayout
   let rec find_and_decorate_Tagged =
           (decorate: ('tag, t('tag)) => decorate_result('tag), l: t('tag))
           : option(t('tag)) => {
     let go = find_and_decorate_Tagged(decorate);
     switch (l) {
     | Linebreak
     | Text(_) => None
     | Align(l) => l |> go |> Opt.map(align)
     | Cat(l1, l2) =>
       switch (l1 |> go) {
       | Some(l1) => Some(Cat(l1, l2))
       | None => l2 |> go |> Opt.map(l2 => Cat(l1, l2))
       }
     | Tagged(tg, l) =>
       switch (decorate(tag, l)) {
       | Failed => None
       | Skipped => l |> go |> Opt.map(tag(tg))
       | Decorated(l) => Some(l)
       }
     };
   };
   */
