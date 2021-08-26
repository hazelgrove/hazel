open UHDoc_common;

let mk_Tag = (t: string): UHDoc.t =>
  mk_text(t) |> annot_Tessera |> annot_Operand(~sort=Tag);

let mk_TagHole = (u: MetaVar.t): UHDoc.t =>
  Int.to_string(u + 1)
  |> Delim.empty_hole_doc
  |> annot_Tessera
  |> annot_Operand(~sort=Tag);

let mk_TagArg = (body: formatted_child): UHDoc.t => {
  let open_group = Delim.open_Parenthesized() |> annot_Tessera;
  let close_group = Delim.close_Parenthesized() |> annot_Tessera;
  Doc.hcats([open_group, body |> pad_delimited_open_child, close_group])
  |> annot_Operand(~sort=Tag);
};

let mk_ArgTag = (tag: formatted_child, body: formatted_child): UHDoc.t => {
  let tag_doc = pad_closed_child(~sort=Tag, tag);
  let body_doc = mk_TagArg(body);
  Doc.hcats([tag_doc, body_doc]) |> annot_Tessera |> annot_Operand(~sort=Tag);
};

let mk = {
  lazy(
    memoize((~memoize as _: bool, ~enforce_inline as _: bool, tag: UHTag.t) =>
      switch (tag) {
      | Tag(t) => mk_Tag(t)
      | TagHole(u) => mk_TagHole(u)
      }
    )
  );
};

let mk_child =
    (~memoize: bool, ~enforce_inline: bool, ~child_step: int, tag: UHTag.t)
    : formatted_child => {
  let formattable = (~enforce_inline: bool) =>
    Lazy.force(mk, ~memoize, ~enforce_inline, tag) |> annot_Step(child_step);
  enforce_inline
    ? EnforcedInline(formattable(~enforce_inline))
    : Unformatted(formattable);
};
