open GeneralUtil;

[@deriving sexp]
type tag = TermTag.t;

[@deriving sexp]
type t = Layout.t(tag);

let result_of_opt: option(t) => Layout.decorate_result(tag) =
  fun
  | None => Failed
  | Some(x) => Decorated(x);

// TODO should be possible to make polymorphic over tag
// but was getting confusing type inference error
let rec find_and_decorate_Tagged =
        (decorate: (tag, t) => Layout.decorate_result(tag), l: t): option(t) => {
  let go = find_and_decorate_Tagged(decorate);
  switch (l) {
  | Linebreak
  | Text(_) => None
  | Align(l) => l |> go |> Opt.map(Layout.align)
  | Cat(l1, l2) =>
    switch (l1 |> go) {
    | Some(l1) => Some(Cat(l1, l2))
    | None => l2 |> go |> Opt.map(l2 => Layout.Cat(l1, l2))
    }
  | Tagged(tag, l) =>
    switch (decorate(tag, l)) {
    | Failed => None
    | Skipped => l |> go |> Opt.map(Layout.tag(tag))
    | Decorated(l) => Some(l)
    }
  };
};

let rec follow_steps_and_decorate =
        (~steps: CursorPath.steps, ~decorate: t => option(t), l: t)
        : option(t) => {
  let go = follow_steps_and_decorate(~decorate);
  switch (steps) {
  | [] => decorate(l)
  | [next_step, ...rest] =>
    l
    |> find_and_decorate_Tagged((tag: TermTag.t, l: t) => {
         switch (tag) {
         | Step(step) when step == next_step =>
           l |> go(~steps=rest) |> Opt.map(Layout.tag(tag)) |> result_of_opt
         | OpenChild(_)
         | ClosedChild(_)
         | DelimGroup
         | Term(_) => Skipped
         | _ => Failed
         }
       })
  };
};

let find_and_decorate_caret =
    (~path as (steps, cursor): CursorPath.t, l: t): option(t) =>
  l
  |> follow_steps_and_decorate(
       ~steps,
       ~decorate=
         switch (cursor) {
         | OnText(j) =>
           find_and_decorate_Tagged((tag, l) =>
             switch (tag) {
             | Text(text_data) =>
               Decorated(
                 l
                 |> Layout.tag(TermTag.Text({...text_data, caret: Some(j)})),
               )
             | _ => Failed
             }
           )
         | OnOp(side) =>
           find_and_decorate_Tagged((tag, l) =>
             switch (tag) {
             | Op(op_data) =>
               Decorated(
                 l
                 |> Layout.tag(TermTag.Op({...op_data, caret: Some(side)})),
               )
             | _ => Failed
             }
           )
         | OnDelim(k, side) =>
           find_and_decorate_Tagged((tag, l) =>
             switch (tag) {
             | Delim({path: (_, index), _} as delim_data) =>
               index == k
                 ? Decorated(
                     l
                     |> Layout.tag(
                          TermTag.Delim({...delim_data, caret: Some(side)}),
                        ),
                   )
                 : Failed
             | DelimGroup => Skipped
             | _ => Failed
             }
           )
         },
     );

// TODO document difference from follow_steps_and_decorate
let rec find_and_decorate_Term =
        (
          ~steps: CursorPath.steps,
          ~decorate_Term: (TermTag.term_data, t) => t,
          l: t,
        )
        : option(t) => {
  let go = find_and_decorate_Term(~decorate_Term);
  switch (steps) {
  | [] =>
    l
    |> find_and_decorate_Tagged((tag, l) =>
         switch (tag) {
         | Term(term_data) => Decorated(decorate_Term(term_data, l))
         | _ => Failed
         }
       )
  | [next_step, ...rest] =>
    l
    |> find_and_decorate_Tagged((tag, l) => {
         let take_step = () =>
           l |> go(~steps=rest) |> Opt.map(Layout.tag(tag)) |> result_of_opt;
         let found_term_if = (cond, term_data) =>
           cond && rest == []
             ? Layout.Decorated(
                 decorate_Term(term_data, l |> Layout.tag(tag)),
               )
             : Skipped;
         switch (tag) {
         | Step(step) => step == next_step ? take_step() : Failed
         | Term({shape: SubBlock({hd_index, _}), _} as term_data) =>
           found_term_if(hd_index == next_step, term_data)
         | Term({shape: NTuple({comma_indices, _}), _} as term_data) =>
           found_term_if(comma_indices |> contains(next_step), term_data)
         | Term({shape: BinOp({op_index, _}), _} as term_data) =>
           found_term_if(op_index == next_step, term_data)
         | OpenChild(_)
         | ClosedChild(_)
         | DelimGroup
         | Term({shape: Operand(_) | Rule, _}) => Skipped
         | _ => Failed
         };
       })
  };
};

let find_and_decorate_cursor =
  find_and_decorate_Term(~decorate_Term=(term_data, l) =>
    l |> Layout.tag(TermTag.Term({...term_data, has_cursor: true}))
  );
