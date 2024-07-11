// let editor_of_code = (~read_only=false, code: CodeString.t) => {
//   switch (Printer.zipper_of_string(code)) {
//   | None => None
//   | Some(z) => Some(Editor.init(~read_only, z))
//   };
// };

let rec append_exp = (e1: TermBase.UExp.t, e2: TermBase.UExp.t) => {
  switch (e1.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | Triv
  | Deferral(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | ListLit(_)
  | Constructor(_)
  | Fun(_)
  | TypFun(_)
  | Tuple(_)
  | Var(_)
  | Ap(_)
  | TypAp(_)
  | DeferredAp(_)
  | Pipeline(_)
  | If(_)
  | Test(_)
  | Parens(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | Match(_) => TermBase.UExp.{ids: [Id.mk()], term: Seq(e1, e2)}
  | Seq(e11, e12) =>
    let e12' = append_exp(e12, e2);
    TermBase.UExp.{ids: e1.ids, term: Seq(e11, e12')};
  | Filter(act, econd, ebody) =>
    let ebody' = append_exp(ebody, e2);
    TermBase.UExp.{ids: e1.ids, term: Filter(act, econd, ebody')};
  | Let(p, edef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    TermBase.UExp.{ids: e1.ids, term: Let(p, edef, ebody')};
  | TyAlias(tp, tdef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    TermBase.UExp.{ids: e1.ids, term: TyAlias(tp, tdef, ebody')};
  };
};
