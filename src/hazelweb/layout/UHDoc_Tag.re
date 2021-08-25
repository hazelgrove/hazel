// TODO: move non-sort-agnostic stuff out of UHDoc_common (make a cleanup issue)
// TODO: memoize this

let mk =
  lazy(
    UHDoc_common.memoize(
      [@warning "-27"] (~memoize: bool, ~enforce_inline: bool, tag: UHTag.t) =>
      switch (tag) {
      | Tag(t) => UHDoc_common.mk_Tag(t)
      | TagHole(u) => UHDoc_common.mk_TagHole(Int.to_string(u))
      }
    )
  );

let mk_child =
    (~memoize: bool, ~enforce_inline: bool, ~child_step: int, tag: UHTag.t)
    : UHDoc_common.formatted_child => {
  let formattable = (~enforce_inline: bool) =>
    Lazy.force(mk, ~memoize, ~enforce_inline, tag)
    |> UHDoc_common.annot_Step(child_step);
  enforce_inline
    ? EnforcedInline(formattable(~enforce_inline))
    : Unformatted(formattable);
};
