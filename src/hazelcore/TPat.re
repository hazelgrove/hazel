/* type variable pattern errors */
module HoleReason = {
  // [@deriving sexp]
  type t =
    | Reserved
    | InvalidName;
};

/* type variable pattern hole status */
module Status = {
  // [@deriving sexp]
  type t =
    | NotInHole
    | InHole(HoleReason.t, MetaVar.t);
};

// [@deriving sexp]
type t =
  | EmptyHole
  | TyVar(Status.t, TyVar.Name.t);

let is_complete =
  fun
  | EmptyHole
  | TyVar(InHole(_), _) => false
  | TyVar(NotInHole, _) => true;

let of_name = (name: TyVar.Name.t, u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (status, u_gen) =
    switch (name |> TyVar.Name.to_string |> ExpandingKeyword.mk) {
    | Some(_) =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (Status.InHole(Reserved, u), u_gen);
    | None =>
      TyVar.Name.builtin(name)
        ? {
          let (u, u_gen) = MetaVarGen.next(u_gen);
          (Status.InHole(Reserved, u), u_gen);
        }
        : (Status.NotInHole, u_gen)
    };
  (TyVar(status, name), u_gen);
};

let binds_tyvar = (name: TyVar.Name.t): (t => bool) =>
  fun
  | EmptyHole
  | TyVar(InHole(_), _) => false
  | TyVar(NotInHole, name') => TyVar.Name.equal(name, name');
