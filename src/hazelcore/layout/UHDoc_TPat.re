module Make = (Memo: Memo.S) => {
  let mk =
    lazy(
      Memo.memoize((~memoize: bool, ~enforce_inline: bool, tp: TPat.t) => {
        let _ = enforce_inline;
        let _ = memoize;
        switch (tp) {
        | EmptyHole => UHDoc_common.mk_EmptyHole("?", ~sort=Typ)
        | InvalidText(_, t)
        | TyVar(_, t) => UHDoc_common.mk_Var(~sort=Typ, t)
        };
      })
    );

  let mk_child =
      (~memoize: bool, ~enforce_inline: bool, ~child_step: int, tp: TPat.t)
      : UHDoc_common.formatted_child => {
    let formattable = (~enforce_inline: bool) =>
      Lazy.force(mk, ~memoize, ~enforce_inline, tp)
      |> UHDoc_common.annot_Step(child_step);
    enforce_inline
      ? EnforcedInline(formattable(~enforce_inline=true))
      : Unformatted(formattable);
  };
};
