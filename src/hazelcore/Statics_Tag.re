let syn_fix_holes =
    (
      _ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      tag: UHTag.t,
    )
    : (UHTag.t, MetaVarGen.t) =>
  switch (tag) {
  | Tag(_) => (tag, u_gen)
  | EmptyTagHole(_) =>
    if (renumber_empty_holes) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (EmptyTagHole(u), u_gen);
    } else {
      (tag, u_gen);
    }
  };

let ana_fix_holes =
    (
      _ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      tag: UHTag.t,
      ty: HTyp.t,
    )
    : (UHTag.t, MetaVarGen.t) =>
  switch (tag) {
  | Tag(_, t) =>
    if (!UHTag.is_tag_name(t)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (Tag(InTagHole(InvalidName, u), t), u_gen);
    } else {
      switch (ty) {
      | Sum(Finite(tymap)) =>
        switch (TagMap.find_opt(tag, tymap)) {
        | Some(_) => (Tag(NotInTagHole, t), u_gen)
        | None =>
          let (u, u_gen) = MetaVarGen.next(u_gen);
          (Tag(InTagHole(NotInSum, u), t), u_gen);
        }
      | _ => (Tag(NotInTagHole, t), u_gen)
      };
    }
  | EmptyTagHole(_) =>
    if (renumber_empty_holes) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (EmptyTagHole(u), u_gen);
    } else {
      (tag, u_gen);
    }
  };
