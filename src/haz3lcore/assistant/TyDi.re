open Util.OptUtil.Syntax;
open Suggestion;

/* Suggest the token at the top of the backpack, if we can put it down */
let suggest_backpack = (z: Zipper.t): list(Suggestion.t) => {
  /* Note: Sort check unnecessary here as wouldn't be able to put down */
  switch (z.backpack) {
  | [] => []
  | [{content, _}, ..._] =>
    switch (content) {
    | [Tile({label, shards: [idx], _})] when Zipper.can_put_down(z) => [
        {content: List.nth(label, idx), strategy: Any(FromBackpack)},
      ]
    | _ => []
    }
  };
};

let suggest = (ci: Info.t, z: Zipper.t): list(Suggestion.t) => {
  /* NOTE: Sorting ensures that if we have an exact match already,
   * we won't suggest extending it, but straight-up lexical sorting
   * may not be desirable in other ways, for example maybe we want
   * recency bias in ctx. Revisit this later. I'm sorting before
   * combination because we want backpack candidates to show up first */
  suggest_backpack(z)
  @ (
    AssistantForms.suggest_operand(ci)
    @ AssistantForms.suggest_leading(ci)
    @ AssistantCtx.suggest_variable(ci)
    @ AssistantCtx.suggest_lookahead_variable(ci)
    |> List.sort(Suggestion.compare)
  )
  @ (AssistantForms.suggest_operator(ci) |> List.sort(Suggestion.compare));
};

/* If there is a monotile to the left of the caret, return it. We
 * currently only make suggestions in such situations */
let token_to_left = (z: Zipper.t): option(string) =>
  switch (
    z.caret,
    z.relatives.siblings |> fst |> List.rev,
    z.relatives.siblings |> snd,
  ) {
  | (Outer, [Tile({label: [tok_to_left], _}), ..._], _) =>
    Some(tok_to_left)
  | _ => None
  };

/* The selection buffer used by TyDi is currently unstructured; it simply
 * holds an unparsed string, which is parsed via the same mechanism as
 * Paste only when a suggestion is accepted. */
let mk_unparsed_buffer =
    (~sort: Sort.t, sibs: Siblings.t, t: Token.t): Segment.t => {
  let mold = Siblings.mold_fitting_between(sort, Precedence.max, sibs);
  [Tile({id: Id.mk(), label: [t], shards: [0], children: [], mold})];
};

/* If 'current' is a proper prefix of 'candidate', return the
 * suffix such that current ++ suffix == candidate */
let suffix_of = (candidate: Token.t, current: Token.t): option(Token.t) => {
  let candidate_suffix =
    String.sub(
      candidate,
      String.length(current),
      String.length(candidate) - String.length(current),
    );
  candidate_suffix == "" ? None : Some(candidate_suffix);
};

/* PERF: This is quite expensive */
let z_to_ci = (~settings: CoreSettings.t, ~ctx: Ctx.t, z: Zipper.t) => {
  let map =
    z
    |> MakeTerm.from_zip_for_sem
    |> fst
    |> Interface.Statics.mk_map_ctx(settings, ctx);
  let* index = Indicated.index(z);
  Id.Map.find_opt(index, map);
};

/* Returns the text content of the suggestion buffer */
let get_buffer = (z: Zipper.t): option(Token.t) =>
  switch (z.selection.mode, z.selection.content) {
  | (Buffer(Unparsed), [Tile({label: [completion], _})]) =>
    Some(completion)
  | _ => None
  };

/* Populates the suggestion buffer with a type-directed suggestion */
let set_buffer = (~settings, ~ctx: Ctx.t, z: Zipper.t): option(Zipper.t) => {
  let* tok_to_left = token_to_left(z);
  let* ci = z_to_ci(~settings, ~ctx, z);
  let suggestions = suggest(ci, z);
  let suggestions =
    suggestions
    |> List.filter(({content, _}: Suggestion.t) =>
         String.starts_with(~prefix=tok_to_left, content)
         /* HACK(andrew): Below filtering of cons suggestion
          * should be done in a more principled way when a
          * ranker is implemented */
         && !(Info.sort_of(ci) == Pat && content == "::")
       );
  let* top_suggestion = suggestions |> Util.ListUtil.hd_opt;
  let* suggestion_suffix = suffix_of(top_suggestion.content, tok_to_left);
  let content =
    mk_unparsed_buffer(
      ~sort=Info.sort_of(ci),
      z.relatives.siblings,
      suggestion_suffix,
    );
  let z = Zipper.set_buffer(z, ~content, ~mode=Unparsed);
  Some(z);
};
