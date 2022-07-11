let syn_fix_holes =
    (
      _ctx: Contexts.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      tag: UHTag.t,
    )
    : (UHTag.t, IDGen.t) =>
  switch (tag) {
  | Tag(_) => (tag, id_gen)
  | EmptyTagHole(_) =>
    if (renumber_empty_holes) {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (EmptyTagHole(u), id_gen);
    } else {
      (tag, id_gen);
    }
  };

let ana_fix_holes =
    (
      _ctx: Contexts.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      tag: UHTag.t,
      ty: HTyp.t,
    )
    : (UHTag.t, IDGen.t) =>
  switch (tag) {
  | Tag(_, t) =>
    if (!UHTag.is_tag_name(t)) {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (Tag(InTagHole(InvalidName, u), t), id_gen);
    } else {
      switch (ty) {
      | Sum(Finite(tymap)) =>
        switch (TagMap.find_opt(tag, tymap)) {
        | Some(_) => (Tag(NotInTagHole, t), id_gen)
        | None =>
          let (u, id_gen) = IDGen.next_hole(id_gen);
          (Tag(InTagHole(NotInSum, u), t), id_gen);
        }
      | _ => (Tag(NotInTagHole, t), id_gen)
      };
    }
  | EmptyTagHole(_) =>
    if (renumber_empty_holes) {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (EmptyTagHole(u), id_gen);
    } else {
      (tag, id_gen);
    }
  };
