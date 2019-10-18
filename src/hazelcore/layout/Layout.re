open Sexplib.Std;

/* Variable: `layout` */
[@deriving sexp]
type t('tag) =
  | Text(string) // Invariant: contains no newlines. Text("") is identity for `Cat`
  | Cat(t('tag), t('tag)) // associative
  | Linebreak
  | Align(t('tag))
  | Tagged('tag, t('tag));

let string_of_layout: 'tag. t('tag) => string =
  layout => {
    let output: ref(list(string)) = ref([]); // Stored in revese order
    let column: ref(int) = ref(0);
    let print = (string: string): unit => output := [string, ...output^];
    let rec go: 'tag. (int, t('tag)) => unit =
      indent => {
        fun
        | Text(string) => {
            column := column^ + String.length(string);
            print(string);
          }
        | Cat(l1, l2) => {
            go(indent, l1);
            go(indent, l2);
          }
        | Linebreak => {
            print("\n");
            print(String.make(indent, ' '));
            column := indent;
          }
        | Align(l) => {
            go(column^, l);
          }
        | Tagged(_tag, l) => go(indent, l);
      };
    go(0, layout);
    String.concat("", List.rev(output^));
  };
