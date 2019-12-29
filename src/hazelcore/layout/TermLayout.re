open GeneralUtil;

[@deriving sexp]
type tag = TermTag.t;

[@deriving sexp]
type t = Layout.t(tag);

let has_inline_OpenChild = (l: t): bool => {
  let rec go = l =>
    Layout.query_next_Tagged(
      (_, tag: TermTag.t, l1) =>
        switch (tag) {
        | Step(_)
        | DelimGroup => go(l1)
        | OpenChild({is_inline: true}) => Some()
        | _ => None
        },
      l,
    );
  go(l) |> Opt.test;
};

let has_para_OpenChild = (l: t) => {
  let rec go = l =>
    Layout.query_next_Tagged(
      (_, tag: TermTag.t, l1) =>
        switch (tag) {
        | Step(_)
        | DelimGroup => go(l1)
        | OpenChild({is_inline: false}) => Some()
        | _ => None
        },
      l,
    );
  go(l) |> Opt.test;
};

let rec follow_steps_and_decorate =
        (~steps: CursorPath.steps, ~decorate: t => option(t), l: t)
        : option(t) => {
  let go = follow_steps_and_decorate(~decorate);
  switch (steps) {
  | [] => decorate(l)
  | [next_step, ...rest] =>
    l
    |> Layout.query_next_Tagged((metrics, tag: TermTag.t, l) =>
         switch (tag) {
         | Step(step) when step == next_step =>
           l |> go(~steps=rest) |> Opt.map(Layout.mk_Tagged(metrics, tag))
         | OpenChild(_)
         | ClosedChild(_)
         | DelimGroup
         | Term(_) =>
           l |> go(~steps) |> Opt.map(Layout.mk_Tagged(metrics, tag))
         | _ => None
         }
       )
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
           let rec find_Text = l =>
             Layout.query_next_Tagged(
               (metrics, tag: TermTag.t, l1) =>
                 switch (tag) {
                 | Text(text_data) =>
                   Some(
                     Layout.mk_Tagged(
                       metrics,
                       TermTag.Text({...text_data, caret: Some(j)}),
                       l1,
                     ),
                   )
                 | Term(_) => find_Text(l1)
                 | _ => None
                 },
               l,
             );
           find_Text;
         | OnOp(side) =>
           Layout.query_next_Tagged((metrics, tag: TermTag.t, l) =>
             switch (tag) {
             | Op(op_data) =>
               Some(
                 Layout.mk_Tagged(
                   metrics,
                   TermTag.Op({...op_data, caret: Some(side)}),
                   l,
                 ),
               )
             | _ => None
             }
           )
         | OnDelim(k, side) =>
           let rec find_Delim = l =>
             Layout.query_next_Tagged(
               (metrics, tag: TermTag.t, l1) =>
                 switch (tag) {
                 | Delim({path: (_, index), _} as delim_data)
                     when index == k =>
                   Some(
                     Layout.mk_Tagged(
                       metrics,
                       TermTag.Delim({...delim_data, caret: Some(side)}),
                       l1,
                     ),
                   )
                 | Term(_)
                 | DelimGroup => find_Delim(l1)
                 | _ => None
                 },
               l,
             );
           find_Delim;
         },
     );

// TODO document difference from follow_steps_and_decorate
let rec find_and_decorate_Term =
        (
          ~steps: CursorPath.steps,
          ~decorate_Term: (Layout.metrics, TermTag.term_data, t) => t,
        )
        : (t => option(t)) => {
  let go = find_and_decorate_Term(~decorate_Term);
  switch (steps) {
  | [] =>
    Layout.query_next_Tagged((metrics, tag: TermTag.t, l) =>
      switch (tag) {
      | Term(term_data) => Some(decorate_Term(metrics, term_data, l))
      | _ => None
      }
    )
  | [next_step, ...rest] =>
    let rec find_Step = l =>
      Layout.query_next_Tagged(
        (metrics, tag, l1) => {
          let found_term_if = (cond, term_data) =>
            cond && rest == []
              ? Some(decorate_Term(metrics, term_data, l1)) : find_Step(l1);
          switch (tag) {
          | Step(step) when step == next_step =>
            l1 |> go(~steps=rest) |> Opt.map(Layout.mk_Tagged(metrics, tag))
          | Term({shape: SubBlock({hd_index, _}), _} as term_data) =>
            found_term_if(hd_index == next_step, term_data)
          | Term({shape: NTuple({comma_indices, _}), _} as term_data) =>
            found_term_if(
              comma_indices |> GeneralUtil.contains(next_step),
              term_data,
            )
          | Term({shape: BinOp({op_index, _}), _} as term_data) =>
            found_term_if(op_index == next_step, term_data)
          | OpenChild(_)
          | ClosedChild(_)
          | DelimGroup
          | Term({shape: Operand(_) | Rule, _}) => find_Step(l1)
          | _ => None
          };
        },
        l,
      );
    find_Step;
  };
};

let find_and_decorate_cursor =
  find_and_decorate_Term(~decorate_Term=(metrics, term_data) =>
    Layout.mk_Tagged(metrics, TermTag.Term({...term_data, has_cursor: true}))
  );
