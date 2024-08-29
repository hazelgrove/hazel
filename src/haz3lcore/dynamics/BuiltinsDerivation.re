[@deriving (show({with_path: false}), sexp, yojson)]
type term =
  | Atom
  | And
  | Or
  | Implies
  | Truth
  | Falsity
  | Entail;

let all = [Atom, And, Or, Implies, Truth, Falsity, Entail];

let to_term = s =>
  List.fold_left(
    (acc, ctr) =>
      Option.is_none(acc) && show_term(ctr) == s ? Some(ctr) : acc,
    None,
    all,
  );

let to_arg_typ: term => option(Typ.t) =
  fun
  | Atom => Some(String |> Typ.fresh)
  | And
  | Or
  | Implies =>
    Some(Prod([Prop |> Typ.fresh, Prop |> Typ.fresh]) |> Typ.fresh)
  | Truth
  | Falsity => None
  | Entail => Some(List(Prop |> Typ.fresh) |> Typ.fresh);

let to_typ: term => Typ.t =
  term =>
    switch (to_arg_typ(term)) {
    | Some(ty) => Arrow(ty, Prop |> Typ.fresh) |> Typ.fresh
    | None => Prop |> Typ.fresh
    };

let mk_ctr_entry: term => Ctx.var_entry =
  term => {name: show_term(term), id: Id.mk(), typ: to_typ(term)};

let ctr_entries = all |> List.map(mk_ctr_entry);

// let mk_variant: term => ConstructorMap.variant(Typ.t) =
//   term => {
//     Variant(show_term(term), [Id.mk()], to_arg_typ(term));
//   };

// let tvar_entry =
//   Ctx.TVarEntry({
//     name: "P",
//     id: Id.invalid,
//     kind: Ctx.Singleton(Sum(all |> List.map(mk_variant)) |> Typ.fresh),
//   });

let transit: (term, option(DHExp.t)) => DHExp.t =
  (term, d) => {
    switch (d) {
    | None =>
      switch (term) {
      | Truth
      | Falsity =>
        let prop: Derivation.Prop.t =
          switch (term) {
          | Truth => Truth
          | Falsity => Falsity
          | _ => failwith("Impossible")
          };
        DHExp.Prop(Prop(prop)) |> DHExp.fresh;
      | _ => failwith("impossible")
      }
    | Some(d) =>
      switch (term, DHExp.term_of(d)) {
      | (Atom, String(atom)) =>
        let prop: Derivation.Prop.t = Atom(atom);
        DHExp.Prop(Prop(prop)) |> DHExp.fresh;
      | (
          And | Or | Implies,
          Tuple([{term: Prop(Prop(p1)), _}, {term: Prop(Prop(p2)), _}]),
        ) =>
        let prop: Derivation.Prop.t =
          switch (term) {
          | And => And(p1, p2)
          | Or => Or(p1, p2)
          | Implies => Implies(p1, p2)
          | _ => failwith("Impossible")
          };
        DHExp.Prop(Prop(prop)) |> DHExp.fresh;
      | (Entail, Tuple([{term: ListLit(l), _}, {term: Prop(Prop(p)), _}]))
          when
            l
            |> List.for_all(p =>
                 switch (DHExp.term_of(p)) {
                 | Prop(Prop(_)) => true
                 | _ => false
                 }
               ) =>
        let ctx: list(Derivation.Prop.t) =
          l
          |> List.map(p =>
               switch (DHExp.term_of(p)) {
               | Prop(Prop(p)) => p
               | _ => failwith("impossible")
               }
             );
        let jdmt: Derivation.Judgement.t = Entail(ctx, p);
        DHExp.Prop(Judgement(jdmt)) |> DHExp.fresh;
      | _ => raise(EvaluatorError.Exception(InvalidBoxedTuple(d)))
      }
    };
  };
