type t =
  | Hole(MetaVar.t)
  | Text(string)
  | List_l
  | List_r
  | Paren_l
  | Paren_r
  | Inj_l
  | Inj_r
  | Case_case
  | Case_bar
  | Case_arrow
  | Case_end
  | Fun_fun
  | Fun_arrow
  | Let_let
  | Let_eq
  | Let_in;