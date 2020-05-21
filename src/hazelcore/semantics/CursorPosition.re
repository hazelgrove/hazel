// TODO(undergrad): Rename to CaretPosition
// (including updating all variable
// naming that assumed cursor position).
// Talk to @d before starting.
[@deriving sexp]
type t =
  | OnText(CharIndex.t)
  | OnDelim(DelimIndex.t, Side.t)
  | OnOp(Side.t);

let text_cursors = (len: int): list(t) =>
  ListUtil.range(len + 1) |> List.map(j => OnText(j));

let delim_cursors_k = (k: int): list(t) => [
  OnDelim(k, Before),
  OnDelim(k, After),
];
let delim_cursors = (num_delim: int): list(t) =>
  ListUtil.range(num_delim)
  |> List.map(k => delim_cursors_k(k))
  |> List.flatten;

let force_get_OnText =
  fun
  | OnDelim(_, _)
  | OnOp(_) => failwith("force_get_OnText: expected OnText")
  | OnText(j) => j;

/* let rec find_cursor = (ze: ZExp.t): option(t) => ze |> find_cursor_zblock
   and find_cursor_zblock = ((_, zline, _): ZExp.zblock): t =>
     zline |> find_cursor_zline
   and find_cursor_zline =
     fun
     | CursorL(_) => None
     | ExpLineZ(zopseq) => zopseq |> find_cursor_zopseq
     | LetLineZP(_)
     | LetLineZA(_) => None
     | LetLineZE(_, _, zdef) => zdef |> find_cursor
   and find_cursor_zopseq =
     fun
     | ZOpSeq(_, ZOperand(zoperand, _)) => zoperand |> find_cursor_zoperand
     | ZOpSeq(_, ZOperator(_)) => None
   and find_cursor_zoperator =
     fun
     | _ => None
   and find_cursor_zoperand =
     fun
     | CursorE(cursor, StringLit(_, _)) => option(cursor)
     | ParenthesizedZ(zbody) => find_cursor(zbody)
     | LamZP(_)
     | LamZA(_) => None
     | LamZE(_, _, _, zbody) => find_cursor(zbody)
     | InjZ(_, _, zbody) => find_cursor(zbody)
     | CaseZE(_, zbody, _, _) => find_cursor(zbody)
     | CaseZR(_)
     | CaseZA(_) => None
     | ApPaletteZ(_, _, _, zpsi) => {
         let zhole_map = zpsi.zsplice_map;
         let (n, (_, ze)) = ZNatMap.prj_z_kv(zhole_map);
         find_cursor(ze);
       }
     | SubscriptZE1(_, zbody, _, _) => find_cursor(zbody)
     | SubscriptZE2(_, _, zbody, _) => find_cursor(zbody)
     | SubscriptZE3(_, _, _, zbody) => find_cursor(zbody); */
