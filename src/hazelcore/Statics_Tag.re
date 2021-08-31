let fix_holes =
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
