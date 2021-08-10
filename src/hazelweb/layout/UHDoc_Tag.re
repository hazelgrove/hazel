let mk: UHTag.t => UHDoc.t =
  fun
  | Tag(t) => UHDoc_common.mk_Tag(t)
  | TagHole(_) => UHDoc_common.mk_TagHole("?");
