open Util.OptUtil.Syntax;
open TyDiSuggestion;
open Language;

/* Minimum number of characters required before showing completions.
 * Adjust this value to control when suggestions first appear. */
let min_prefix_len = 2;

/* Suggest the token at the top of the backpack, if we can put it down */
let suggest_backpack = (z: Zipper.t): list(t) => {
  /* Note: Sort check unnecessary here as wouldn't be able to put down */
  switch (Zipper.local_backpack(z)) {
  | [] => []
  | [t, ..._] =>
    switch (t) {
    | {label, shards: [idx], _} when Zipper.can_put_down(z) => [
        {
          content: List.nth(label, idx),
          strategy: Any(FromBackpack),
        },
      ]
    | _ => []
    }
  };
};

/* Check if the expected type is unknown (no type annotation context) */
let has_unknown_expectation = (ci: Info.t): bool =>
  switch (ci) {
  | InfoExp({ana, _})
  | InfoPat({ana, _}) =>
    switch (Typ.term_of(ana)) {
    | Unknown(_) => true
    | _ => false
    }
  | _ => false
  };

let suggest = (ci: Info.t, z: Zipper.t): list(t) => {
  /* NOTE: Sorting ensures that if we have an exact match already,
   * we won't suggest extending it, but straight-up lexical sorting
   * may not be desirable in other ways, for example maybe we want
   * recency bias in ctx. Revisit this later. I'm sorting before
   * combination because we want backpack candidates to show up first */
  switch (ci) {
  | InfoExp({cls: Exp(Label), _})
  | InfoPat({cls: Pat(Label), _})
  | InfoTyp({cls: Typ(Label), _})
  | InfoExp({cls: Exp(TupLabel), _})
  | InfoPat({cls: Pat(TupLabel), _})
  | InfoTyp({cls: Typ(TupLabel), _}) => [] // TODO: Autocomplete for labels
  | _ =>
    /* When the expected type is unknown (e.g., no type annotation),
     * prioritize keywords/forms over context variables. This prevents
     * e.g. 'f' completing to 'false' when the user likely wants 'fun'. */
    let forms =
      TyDiForms.suggest_leading(ci)
      @ TyDiForms.suggest_operand(ci)
      |> List.sort(TyDiSuggestion.compare);
    let ctx_suggestions =
      TyDiCtx.suggest_variable(ci)
      @ TyDiCtx.suggest_lookahead_variable(ci)
      |> List.sort(TyDiSuggestion.compare);
    let operators =
      TyDiForms.suggest_operator(ci) |> List.sort(TyDiSuggestion.compare);
    if (has_unknown_expectation(ci)) {
      /* Unknown type: keywords first, then context, then operators */
      suggest_backpack(z) @ forms @ ctx_suggestions @ operators;
    } else {
      /* Known type: context variables first (type-directed), then forms */
      suggest_backpack(z) @ ctx_suggestions @ forms @ operators;
    };
  };
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
let mk_unparsed_buffer = (t: Token.t): Segment.t => {
  [
    Secondary({
      id: Id.mk(),
      content: Comment(t),
    }),
  ];
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

/* Returns the text content of the suggestion buffer */
let get_unparsed_buffer = (z: Zipper.t): option(Token.t) =>
  switch (z.selection.mode, z.selection.content) {
  | (Buffer(Unparsed), [Secondary({content: Comment(completion), _})]) =>
    Some(completion)
  | _ => None
  };

/* Populates the suggestion buffer with a type-directed suggestion */
let set_buffer = (~ci: option(Info.t), z: Zipper.t): option(Zipper.t) => {
  let* ci = ci;
  let* _ =
    switch (z.selection.mode) {
    /* Make sure not to populate the completion buffer if there is a non-empty
     * selection, otherwise it will get clobbered by the buffer */
    | Buffer(Unparsed | Parsed) => Some()
    | Normal when Selection.is_empty(z.selection) => Some()
    | Normal => None
    };
  let* tok_to_left = token_to_left(z);
  /* Only show completions after typing enough characters */
  let* _ = String.length(tok_to_left) >= min_prefix_len ? Some() : None;
  let suggestions = suggest(ci, z);
  let suggestions =
    suggestions
    |> List.filter(({content, _}: TyDiSuggestion.t) =>
         String.starts_with(~prefix=tok_to_left, content)
       );
  let* top_suggestion = suggestions |> Util.ListUtil.hd_opt;
  let* suggestion_suffix = suffix_of(top_suggestion.content, tok_to_left);
  let content = mk_unparsed_buffer(suggestion_suffix);
  let z = Zipper.set_buffer(z, ~content, ~mode=Unparsed);
  Some(z);
};
