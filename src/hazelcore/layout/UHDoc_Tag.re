open UHDoc_common;

let mk_Tag = (t: string): UHDoc.t =>
  mk_text(t) |> annot_Tessera |> annot_Operand(~sort=Tag);

let mk_TagHole = (u: MetaVar.t): UHDoc.t =>
  Int.to_string(u + 1)
  |> Delim.empty_hole_doc
  |> annot_Tessera
  |> annot_Operand(~sort=Tag);

module Make = (Memo: Memo.S) => {
  let mk = {
    lazy(
      Memo.memoize(
        (~memoize as _: bool, ~enforce_inline as _: bool, tag: UHTag.t) =>
        switch (tag) {
        | Tag(_, t) => mk_Tag(t)
        | EmptyTagHole(u) => mk_TagHole(u)
        }
      )
    );
  };

  /* same as mk_child but with no Step annotation */
  let mk_formatted =
      (~memoize: bool, ~enforce_inline: bool, tag: UHTag.t): formatted_child => {
    let formattable = (~enforce_inline: bool) =>
      Lazy.force(mk, ~memoize, ~enforce_inline, tag);
    enforce_inline
      ? EnforcedInline(formattable(~enforce_inline))
      : Unformatted(formattable);
  };

  let mk_child =
      (~memoize: bool, ~enforce_inline: bool, ~child_step: int, tag: UHTag.t)
      : formatted_child => {
    let formattable = (~enforce_inline: bool) =>
      Lazy.force(mk, ~memoize, ~enforce_inline, tag)
      |> annot_Step(child_step);
    enforce_inline
      ? EnforcedInline(formattable(~enforce_inline))
      : Unformatted(formattable);
  };
};
