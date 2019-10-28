open Sexplib.Std;

/* Variable: `layout` */
[@deriving sexp]
type t('tag) =
  | Text(string) // Invariant: contains no newlines. Text("") is identity for `Cat`
  | Cat(t('tag), t('tag)) // associative
  | Linebreak
  | Align(t('tag))
  | Tagged('tag, t('tag));

type text('tag, 'imp, 't) = {
  imp_of_string: string => 'imp,
  imp_of_tag: ('tag, 'imp) => 'imp,
  imp_append: ('imp, 'imp) => 'imp,
  imp_newline: 'imp,
  t_of_imp: 'imp => 't,
};

let t_of_layout: (text('tag, 'imp, 't), t('tag)) => 't =
  (text, layout) => {
    //let output: ref(list(string)) = ref([]); // Stored in reverse order
    let column: ref(int) = ref(0);
    //let print = (string: string): unit => output := [string, ...output^];
    let rec go: (int, t('tag)) => 'imp =
      indent => {
        fun
        | Text(string) => {
            column := column^ + String.length(string);
            text.imp_of_string(string);
          }
        | Cat(l1, l2) => {
            text.imp_append(go(indent, l1), go(indent, l2));
          }
        | Linebreak => {
            // TODO: no indent if on final line break
            column := indent;
            text.imp_append(
              text.imp_newline,
              text.imp_of_string(String.make(indent, ' ')),
            );
          }
        | Align(l) => {
            go(column^, l);
          }
        | Tagged(tag, l) => text.imp_of_tag(tag, go(indent, l));
      };
    text.t_of_imp(go(0, layout));
    //String.concat("", List.rev(output^));
  };

let string_of_layout: 'tag. t('tag) => string =
  layout => {
    let record: 'tag. text('tag, string, string) = {
      imp_of_string: string => string,
      imp_of_tag: (_, string) => string,
      imp_append: (s1, s2) => s1 ++ s2,
      imp_newline: "\n",
      t_of_imp: s => s,
    };
    t_of_layout(record, layout);
  };
