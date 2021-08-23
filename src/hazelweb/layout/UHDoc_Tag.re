let mk: UHTag.t => UHDoc.t =
  fun
  | Tag(t) => UHDoc_common.mk_Tag(t)
  | TagHole(u) => UHDoc_common.mk_TagHole(Int.to_string(u));
