let mk =
  lazy(
    UHDoc_common.memoize((~memoize: bool, ~enforce_inline: bool, p: TPat.t) => {
      let _ = enforce_inline;
      let _ = memoize;

      switch (p) {
      | TPat.EmptyHole => UHDoc_common.mk_EmptyHole("?", ~sort=Typ)
      | TPat.TyVar(_, name) =>
        UHDoc_common.mk_Var(~sort=Typ, TyVar.Name.to_string(name))
      };
    })
  );

let mk_child =
    (~memoize: bool, ~enforce_inline: bool, ~child_step: int, p: TPat.t)
    : UHDoc_common.formatted_child => {
  let formattable = (~enforce_inline: bool) =>
    Lazy.force(mk, ~memoize, ~enforce_inline, p)
    |> UHDoc_common.annot_Step(child_step);
  enforce_inline
    ? EnforcedInline(formattable(~enforce_inline=true))
    : Unformatted(formattable);
};
