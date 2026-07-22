open Util.OptUtil.Syntax;
open TyDiSuggestion;
open Language;

/* Minimum number of characters required before showing completions.
 * Adjust this value to control when suggestions first appear. */
let min_prefix_len = 2;

/* Delimiter suggestions come from the completion engine: a witness
   insertion anchored at the token left of the caret IS the engine
   recognizing that token. One recognition source — ghost and quiver
   agree by construction. Syntax-derived: needs no statics. */
let suggest_witnesses = (z: Zipper.t): list(t) =>
  switch (z.caret, z.relatives.siblings |> fst |> List.rev) {
  | (Outer, [Tile({label: [tok], id, _}), ..._]) =>
    let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
    let result = CanonicalCompletion.for_editor(seg);
    result.insertions
    |> List.filter_map((i: CanonicalCompletion.insertion) =>
         Id.equal(i.adjacent_id, id) && i.side == Util.Direction.Right
           ? switch (i.delimiters) {
             | [{typed_len: Some(n), text, _}, ..._]
                 when n == Token.length(tok) =>
               Some({
                 content: text,
                 strategy: Any(FromMissingShards),
               })
             | _ => None
             }
           : None
       );
  | _ => []
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
   * combination because we want missing-shard suggestions to show up first */
  switch (ci) {
  | InfoExp({dot_labels, _}) when dot_labels != [] =>
    List.map(
      label =>
        TyDiSuggestion.{
          content: label,
          strategy: Exp(Common(FromCtx(Label(label) |> Typ.fresh))),
        },
      dot_labels,
    )
  | InfoTyp({
      expects: TypExpectation.LabelProjectionExpected(Some(labels)),
      _,
    })
      when labels != [] =>
    List.map(
      label =>
        TyDiSuggestion.{
          content: label,
          strategy: Typ(FromCtx),
        },
      labels,
    )
  | InfoExp({label_sort: true, _})
  | InfoPat({label_sort: true, _})
  | InfoExp({cls: Exp(Label), _})
  | InfoPat({cls: Pat(Label), _})
  | InfoTyp({cls: Typ(Label), _})
  | InfoExp({cls: Exp(TupLabel), _})
  | InfoPat({cls: Pat(TupLabel), _})
  | InfoTyp({cls: Typ(TupLabel), _}) => []
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
      suggest_witnesses(z) @ forms @ ctx_suggestions @ operators;
    } else {
      /* Known type: context variables first (type-directed), then forms */
      suggest_witnesses(z) @ ctx_suggestions @ forms @ operators;
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
  let* _ =
    switch (z.selection.mode) {
    /* Make sure not to populate the completion buffer if there is a non-empty
     * selection, otherwise it will get clobbered by the buffer */
    | Buffer(Unparsed | Parsed) => Some()
    | Normal when Selection.is_empty(z.selection) => Some()
    | Normal => None
    };
  let* tok_to_left = token_to_left(z);
  /* witness suggestions need no statics — ci is None on exactly the
     states they serve (completion consumed the prefix token) */
  let suggestions =
    switch (ci) {
    | Some(ci) => suggest(ci, z)
    | None => suggest_witnesses(z)
    };
  let suggestions =
    suggestions
    |> List.filter(({content, _}: TyDiSuggestion.t) =>
         String.starts_with(~prefix=tok_to_left, content)
       );
  /* expectation-backed suggestions bypass the length gate: a 1-char
     prefix of a delimiter the syntax expects is high-signal */
  let expectation_backed =
    List.exists(
      ({strategy, _}: TyDiSuggestion.t) =>
        strategy == Any(FromMissingShards),
      suggestions,
    );
  let* _ =
    String.length(tok_to_left) >= min_prefix_len || expectation_backed
      ? Some() : None;
  /* If any suggestion is an exact match for the current token, suppress
   * all suggestions. This check must scan the full list, not just the
   * top suggestion, because exact-match variables and keyword suggestions
   * come from different pipelines and may be ordered differently. */
  let has_exact_match =
    List.exists(
      ({content, _}: TyDiSuggestion.t) => content == tok_to_left,
      suggestions,
    );
  let* _ = has_exact_match ? None : Some();
  let* top_suggestion = suggestions |> Util.ListUtil.hd_opt;
  let* suggestion_suffix = suffix_of(top_suggestion.content, tok_to_left);
  let content = mk_unparsed_buffer(suggestion_suffix);
  let z = Zipper.set_buffer(z, ~content, ~mode=Unparsed);
  Some(z);
};
