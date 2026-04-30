open Zipper;

/* Classification of a line's content with respect to comments */
type line_class =
  | Code
  | Comment
  | Mixed
  | Empty;

/* Classify a segment's pieces: all code, all comment, mixed, or empty.
 * Whitespace (spaces) is ignored for classification purposes. */
let classify = (seg: Segment.t): line_class => {
  let has_code = ref(false);
  let has_comment = ref(false);
  List.iter(
    fun
    | Piece.Secondary(s) when Secondary.is_comment(s) => has_comment := true
    | Piece.Secondary(s) when Secondary.is_linebreak(s) => ()
    | Piece.Secondary(_) => ()
    | Piece.Grout(_) => () /* Grout is structural padding, not code */
    | _ => has_code := true,
    seg,
  );
  switch (has_code^, has_comment^) {
  | (false, false) => Empty
  | (true, false) => Code
  | (false, true) => Comment
  | (true, true) => Mixed
  };
};

/* Extract text from a comment-classified selection, stripping
 * comment delimiters. Whitespace pieces are preserved as-is. */
let uncommented_text = (seg: Segment.t): string =>
  seg
  |> List.map(
       fun
       | Piece.Secondary({content: Language.Secondary.Comment(text), _}) =>
         String.sub(text, 1, String.length(text) - 2)
       | Piece.Secondary({content: Whitespace(ws), _}) => ws
       | _ => "",
     )
  |> String.concat("");

/* Re-insert text character-by-character into a zipper */
let insert_text = (z: t, text: string, ~root): t =>
  if (String.length(text) == 0) {
    z;
  } else {
    let result =
      Token.to_list(text)
      |> List.fold_left(
           (z_opt, c) =>
             switch (z_opt) {
             | None => None
             | Some(z) => Insert.go(c, z, ~root)
             },
           Some(z),
         );
    switch (result) {
    | Some(z) => z
    | None => z
    };
  };

/* Select the current line's content, excluding line breaks.
 * Unselects first, moves to line start, then selects to line end.
 * Uses to_linebreak_raw to land at the literal line edge so the
 * subsequent Select.to_linebreak (which doesn't skip whitespace)
 * covers any leading indent. */
let select_line = (z: t): t => {
  let z = Zipper.unselect(z);
  let z =
    switch (Move.to_linebreak_raw(Left, z)) {
    | Some(z) => z
    | None => z
    };
  switch (Select.to_linebreak(Right, z)) {
  | Some(z) => z
  | None => z
  };
};

/* Uncomment: extract content from comment pieces, destroy
 * selection, and re-insert the content as code. */
let uncomment = (z: t, ~root): option(t) => {
  let text = uncommented_text(z.selection.content);
  let z = Zipper.destroy_selection(z);
  Some(insert_text(z, text, ~root));
};

/* Toggle comment for a single line at the caret position */
let toggle_single = (z: t, ~root): option(t) => {
  let z = select_line(z);
  if (z.selection.content == []) {
    Some(Zipper.unselect(z));
  } else {
    switch (classify(z.selection.content)) {
    | Code => Insert.try_wrap_selection("#", z, ~root)
    | Comment => uncomment(z, ~root)
    | Empty
    | Mixed => Some(Zipper.unselect(z))
    };
  };
};

/* Classify all lines in a segment split by linebreaks.
 * Returns the uniform class if all lines agree, or Mixed. */
let classify_lines = (seg: Segment.t): line_class => {
  /* Split segment on linebreaks into per-line groups */
  let lines =
    List.fold_left(
      (acc, piece) =>
        switch (piece) {
        | Piece.Secondary(s) when Secondary.is_linebreak(s) => [[], ...acc]
        | _ =>
          switch (acc) {
          | [current, ...rest] => [[piece, ...current], ...rest]
          | [] => [[piece]]
          }
        },
      [[]],
      seg,
    )
    |> List.map(List.rev)
    |> List.rev;
  /* Classify each non-empty line and check uniformity */
  let classes = lines |> List.map(classify) |> List.filter(c => c != Empty);
  switch (classes) {
  | [] => Empty
  | [first, ...rest] => List.for_all(c => c == first, rest) ? first : Mixed
  };
};

/* After insert_text, cursor is at end of last line with no selection.
 * Move back to start of first line, then select forward to end of
 * last line, so the toggled region remains selected. */
let reselect_lines = (z: t, num_newlines: int): t => {
  let or_stay = (f, z) =>
    switch (f(z)) {
    | Some(z) => z
    | None => z
    };
  /* Move to start of current (last) line. Use raw variant so we land
   * at the literal line edge (not past leading indent), keeping the
   * subsequent Select.to_linebreak symmetric. */
  let z = or_stay(Move.to_linebreak_raw(Left), z);
  /* Cross linebreaks going left to reach start of first line */
  let z =
    List.fold_left(
      (z, _) =>
        z
        |> or_stay(Move.by_char(Left))
        |> or_stay(Move.to_linebreak_raw(Left)),
      z,
      List.init(num_newlines, Fun.id),
    );
  /* Select to end of current (first) line */
  let z = or_stay(Select.to_linebreak(Right), z);
  /* Cross linebreaks going right, selecting each subsequent line */
  List.fold_left(
    (z, _) =>
      z
      |> or_stay(Select.local(Right))
      |> or_stay(Select.to_linebreak(Right)),
    z,
    List.init(num_newlines, Fun.id),
  );
};

/* Toggle comment for multiple lines spanned by the current selection.
 * Extends selection to cover full lines, then processes each line
 * individually from top to bottom. Result remains selected. */
let toggle_multi = (~deep_reassociate=false, z: t, ~root): option(t) => {
  let maybe_reassoc = deep_reassociate ? Reassociate.go : Fun.id;
  /* Extend selection to cover full lines.
   * Must set focus to match the extension direction,
   * since to_linebreak moves the focus end. */
  let z = Zipper.set_focus(z, Left);
  let z =
    switch (Select.to_linebreak(Left, z)) {
    | Some(z) => z
    | None => z
    };
  let z = Zipper.set_focus(z, Right);
  let z =
    switch (Select.to_linebreak(Right, z)) {
    | Some(z) => z
    | None => z
    };
  let content = z.selection.content;
  switch (classify_lines(content)) {
  | Empty
  | Mixed => Some(Zipper.unselect(z))
  | Code =>
    /* Comment each line: destroy selection, split on newlines,
     * wrap each line in #...#, re-insert with newlines between */
    let text =
      Segment.to_string(
        ~refractor_seg_to_seg=Triggers.refractor_seg_to_seg,
        ~projector_to_segment=Triggers.projector_to_invoke,
        content,
      );
    let z = Zipper.destroy_selection(z);
    let lines = String.split_on_char('\n', text);
    let num_newlines = List.length(lines) - 1;
    let commented =
      lines |> List.map(line => "#" ++ line ++ "#") |> String.concat("\n");
    let z = insert_text(z, commented, ~root);
    let z = maybe_reassoc(z);
    Some(reselect_lines(z, num_newlines));
  | Comment =>
    /* Uncomment each line: destroy selection, extract content
     * from each comment, re-insert with newlines between */
    let text = uncommented_text(content);
    let num_newlines = List.length(String.split_on_char('\n', text)) - 1;
    let z = Zipper.destroy_selection(z);
    let z = insert_text(z, text, ~root);
    let z = maybe_reassoc(z);
    Some(reselect_lines(z, num_newlines));
  };
};

/* Main entry point: toggle line comment.
 * No selection → toggle current line.
 * With selection → toggle all lines the selection spans. */
let go = (~deep_reassociate=false, z: t, ~root): option(t) =>
  if (z.selection.content != []) {
    toggle_multi(~deep_reassociate, z, ~root);
  } else {
    toggle_single(z, ~root);
  };
