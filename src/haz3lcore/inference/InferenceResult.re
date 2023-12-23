type global_inference_info = {
  enabled: bool,
  ctx: Infer.Ctx.t,
};

type suggestion('a) =
  | Solvable('a)
  | NestedInconsistency('a)
  | NoSuggestion(reason_for_silence)
and reason_for_silence =
  | SuggestionsDisabled
  | NonTypeHoleId
  | OnlyHoleSolutions
  | InconsistentSet;

let get_suggestion_text_for_id =
    (id: Id.t, global_inference_info: global_inference_info)
    : suggestion(string) =>
  if (global_inference_info.enabled) {
    let status = Infer.get_suggestion(global_inference_info.ctx, id);
    switch (status) {
    | Some(Solved(Unknown(_)))
    | Some(Unsolved([])) => NoSuggestion(OnlyHoleSolutions)
    | Some(Solved(typ)) => Solvable(Typ.typ_to_string(typ, false))
    | Some(Unsolved([typ])) =>
      NestedInconsistency(Typ.typ_to_string(typ, false))
    | Some(Unsolved(_)) => NoSuggestion(InconsistentSet)
    | None => NoSuggestion(NonTypeHoleId)
    };
  } else {
    NoSuggestion(SuggestionsDisabled);
  };

let hole_nib: Nib.t = {shape: Convex, sort: Any};
let hole_mold: Mold.t = {out: Any, in_: [], nibs: (hole_nib, hole_nib)};

let mk_global_inference_info = (enabled, ctx) => {
  {enabled, ctx};
};

let empty_info = (): global_inference_info =>
  mk_global_inference_info(true, Infer.Ctx.create());