type t('tag) =
  | Choice(list(t('tag)))
  | Box(list(t('tag)))
  | Cat(list(t('tag)))
  | Tag('tag, t('tag))
  | Text(string)
  | Linebreak;

let empty = Text("");
let space = Text(" ");
