let mk_EmptyHole: string => UHDoc_common.t =
  UHDoc_common.mk_EmptyHole(~sort=TPat);

let mk_Var:
  (~err: ErrStatus.t, ~verr: VarErrStatus.t, string) => UHDoc_common.t =
  UHDoc_common.mk_Var(~sort=TPat);

let rec mk =
  lazy(
    UHDoc_common.memoize((~memoize: bool, ~enforce_inline: bool, p: TPat.t) =>
      (Lazy.force(mk_operand, ~memoize, ~enforce_inline, p): UHDoc_common.t)
    )
  )
and mk_operand = {
  let f =
      (~memoize: bool, ~enforce_inline: bool, operand: TPat.operand)
      : UHDoc_common.t => {
    let _ = memoize;
    let _ = enforce_inline;
    switch (operand) {
    | EmptyHole(u) => mk_EmptyHole(UHDoc_common.hole_lbl(u + 1))
    | TyVar(verr, x) => mk_Var(~err=NotInHole, ~verr, x)
    };
  };
  lazy(UHDoc_common.memoize(f));
}
and mk_child =
    (~memoize: bool, ~enforce_inline: bool, ~child_step: int, p: TPat.t)
    : UHDoc_common.formatted_child => {
  let formattable = (~enforce_inline: bool) =>
    Lazy.force(mk, ~memoize, ~enforce_inline, p)
    |> UHDoc_common.annot_Step(child_step);
  enforce_inline
    ? EnforcedInline(formattable(~enforce_inline=true))
    : Unformatted(formattable);
};
