open Derivation.Prop;

let all = [Atom, And, Or, Implies, Truth, Falsity, Entail];

let to_term = s =>
  List.fold_left(
    (acc, ctr) =>
      Option.is_none(acc) && show_cls(ctr) == s ? Some(ctr) : acc,
    None,
    all,
  );

let to_arg_typ: cls => option(Typ.t) =
  fun
  | Atom => Some(String |> Typ.fresh)
  | And
  | Or
  | Implies =>
    Some(Prod([Prop |> Typ.fresh, Prop |> Typ.fresh]) |> Typ.fresh)
  | Truth
  | Falsity => None
  | Entail =>
    Some(
      Prod([List(Prop |> Typ.fresh) |> Typ.fresh, Prop |> Typ.fresh])
      |> Typ.fresh,
    )
  | Ctx => failwith("impossible");

let to_typ: cls => Typ.t =
  term =>
    switch (to_arg_typ(term)) {
    | Some(ty) => Arrow(ty, Prop |> Typ.fresh) |> Typ.fresh
    | None => Prop |> Typ.fresh
    };

let mk_ctr_entry: cls => Ctx.var_entry =
  term => {name: show_cls(term), id: Id.mk(), typ: to_typ(term)};

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

let term_of_dhexp: DHExp.t => option(cls) =
  d =>
    switch (DHExp.term_of(d)) {
    | Constructor(ctr, _) => ctr |> to_term
    | _ => None
    // Need check?
    // Cons: ty |> Typ.term_of == Typ.Prop
    // Ap:    Typ.matched_arrow([], ty)
    //       |> snd
    //       |> Typ.term_of == Typ.Prop
    };

let rec prop_of_dhexp: DHExp.t => Derivation.Prop.t =
  d => {
    let (fn, arg) =
      switch (DHExp.term_of(d)) {
      | Ap(_, fn, arg) => (fn, Some(arg))
      | _ => (d, None)
      };
    switch (term_of_dhexp(fn)) {
    | None =>
      print_endline("Cls not found: " ++ DHExp.show(d));
      Hole("Cls not found");
    | Some(ctr) =>
      switch (ctr, arg) {
      | (Atom, Some({term: String(atom), _})) => Atom(atom)
      | (And | Or | Implies, Some({term: Tuple([d1, d2]), _})) =>
        let (p1, p2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
        switch (ctr) {
        | And => And(p1, p2)
        | Or => Or(p1, p2)
        | Implies => Implies(p1, p2)
        | _ => failwith("Impossible")
        };
      | (Truth, None) => Truth
      | (Falsity, None) => Falsity
      | (Entail, Some({term: Tuple([{term: ListLit(l), _}, d]), _})) =>
        let ctx = List.map(prop_of_dhexp, l);
        let p = prop_of_dhexp(d);
        Entail(Ctx(ctx), p);
      | _ =>
        print_endline("Argument Error: " ++ DHExp.show(d));
        Hole("Argument Error");
      }
    };
  };
