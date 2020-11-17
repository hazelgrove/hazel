[@deriving sexp]
type t = {
  info: CardInfo.t,
  edit_state: Program.EditState.t,
};

let mk = (info: CardInfo.t): t => {
  let edit_state: Program.EditState.t =
    switch (info.init_term) {
    | Unfocused(e) =>
      let (e, ty, u_gen) =
        Statics_Exp.fix_and_renumber_holes(Contexts.empty, e);
      {term: Unfocused(e), ty, u_gen};
    | Focused(ze) =>
      let (ze, ty, u_gen) =
        Statics_Exp.fix_and_renumber_holes_z(Contexts.empty, ze);
      {term: Focused(ze), ty, u_gen};
    };
  {info, edit_state};
};
