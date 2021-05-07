let open_List = "[";
let close_List = "]";

let open_Parenthesized = "(";
let close_Parenthesized = ")";

let sym_Lam = "fun";
let colon_Lam = ":";
let open_Lam = "{";
let close_Lam = "}";
let collapsed_Lam = "<fn>";

let fix_FixF = "fix";
let colon_FixF = ":";
let open_FixF = ".{";
let close_FixF = "}";

let open_Inj: InjSide.t => string =
  (inj_side: InjSide.t) => (
    "inj[" ++ InjSide.to_string(inj_side) ++ "](": string
  );
let open_InjAlt = "(";
let close_Inj = ")";

let open_Case = "case";
let close_Case = "end";
let close_Case_ann = "end :";

let let_LetLine = "let";
let eq_LetLine = "=";
let in_LetLine = "in";

let bar_Rule = "|";
let arrow_Rule = "=>";

let open_Cast = "<";
let arrow_Cast = Unicode.castArrowSym;
let close_Cast = ">";

let open_CommentLine = "#";

let colon_Ann = ":";

let ty_Unit = "()";
let ty_Bool = "Bool";
let ty_Int = "Int";
let ty_Float = "Float";

let exp_Triv = "()";
let exp_Wild = "_";
let exp_ListNil = "[]";

let op_Cons = "::";
let op_Pair = ", ";
