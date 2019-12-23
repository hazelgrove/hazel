let rec decorate_next_Tagged_matching =
        (
          ~decorate: ('tag, Layout.t('tag)) => option(Layout.t('tag)),
          ~matches: 'tag => bool,
          l: Layout.t('tag),
        )
        : option(Layout.t('tag)) => {
  let go = decorate_next_Tagged_matching(~decorate, ~matches);
  switch (l) {
  | Linebreak
  | Text(_) => None
  | Align(l) => go(l) |> Opt.map(Doc.align)
  | Cat(l1, l2) =>
    switch (go(l1), go(l2)) {
    | (None, None) => None
    | (Some(l1), _) => Some(Cat(l1, l2))
    | (_, Some(l2)) => Some(Cat(l1, l2))
    }
  | Tagged(tag, l) =>
    tag |> matches ? decorate(tag, l) : go(l) |> Opt.map(Doc.tag(tag))
  };
};

let decorate_next_Tagged = decorate_next_Tagged_matching(~matches=_ => true);

/**
 * TODO create separate follow_steps and follow_term functions
 *
 * Follows `steps` down `l` and, upon finding the sub-layout
 * corresponding to the minimal term containing the `steps`
 * target, calls `decorate` on the sub-layout and the remaining
 * steps between the minimal term and the `steps` target.
 *
 * The remaining steps passed into `decorate` are used by
 * `decorate_cursor`, in particular, because it needs to
 * decorate both the current term and the caret position.
 */
let rec follow_steps_and_decorate =
        (
          ~steps: CursorPath.steps,
          ~decorate:
             (CursorPath.steps, Layout.t(tag)) => option(Layout.t(tag)),
          l: Layout.t(tag),
        )
        : option(Layout.t(tag)) => {
  let go = follow_steps_and_decorate(~decorate);
  switch (steps) {
  | [] => decorate(l)
  | [next_step, ...rest] =>
    decorate_next_Tagged(
      ~decorate=
        (tag, l) => {
          let found_term = () => l |> Doc.tag(tag) |> decorate(~steps);
          let search_for_next_step = () =>
            l |> go(steps) |> Opt.map(Doc.tag(tag));
          let found_term_if = cond =>
            cond ? found_term() : search_for_next_step();
          let take_step = () => l |> go(rest) |> Opt.map(Doc.tag(tag));
          switch (tag) {
          | Indent
          | Padding
          | HoleLabel
          | SpaceOp(_)
          | Op(_)
          | Delim(_)
          | Text(_) => None

          | OpenChild(_)
          | ClosedChild(_)
          | DelimGroup => search_for_next_step()

          | Step(step) => step == next_step ? take_step() : None

          | Term({shape: SubBlock({hd_index, _}), _}) =>
            found_term_if(hd_index == next_step && rest == [])
          | Term({shape: NTuple({comma_indices, _}), _}) =>
            found_term_if(comma_indices |> contains(next_step))
          | Term({shape: BinOp({op_index, _}), _}) =>
            found_term_if(op_index == next_step)
          | Term({shape: Operand(_) | Rule, _}) => search_for_next_step()
          };
        },
      l,
    )
  };
};

let rec decorate_caret =
        ((caret_steps, cursor): CursorPath.t, l: Layout.t(tag))
        : option(Layout.t(tag)) => {
  l
  |> follow_steps_and_decorate(~steps=caret_steps, ~decorate=(_, stepped_l) =>
       stepped_l
       |> decorate_next_Tagged_matching(
            ~matches=
              switch (cursor) {
              | OnDelim(k, side) => (
                  fun
                  | Delim({path: (_, index), _}) when index == k => true
                  | _ => false
                )
              | _ => (_ => true)
              },
            ~decorate=(tag, l) =>
            switch (cursor, tag) {
            | (OnOp(side), Op(op_data)) =>
              Some(l |> Doc.tag(Op({...op_data, caret: Some(side)})))
            | (OnDelim(k, side), Delim({index, _} as delim_data))
                when k == index =>
              Some(l |> Doc.tag(Delim({...delim_data, caret: Some(side)})))
            | (OnText(j), Text(text_data)) =>
              Some(l |> Doc.tag(Text({...text_data, caret: Some(j)})))
            | _ => None
            }
          )
     );
};

let decorate_cursor =
    ((steps, cursor): CursorPath.t, l: Layout.t(tag))
    : option(Layout.t(tag)) =>
  l
  |> follow_steps_and_decorate(~steps, ~decorate=(caret_steps, term_l) =>
       switch (term_l) {
       | Tagged(Term(term_data), l) =>
         l
         |> decorate_caret((caret_steps, cursor))
         |> Opt.map(Doc.tag(Term({...term_data, has_cursor: true})))
       | _ => None
       }
     );
