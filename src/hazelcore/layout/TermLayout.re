open GeneralUtil;

[@deriving sexp]
type tag = TermTag.t;

[@deriving sexp]
type t = Layout.t(tag);

type metrics = Layout.metrics;

module QueryResult = {
  type t('a) =
    | Stop
    | Skip
    | Return('a);

  let of_opt: option('a) => t('a) =
    fun
    | None => Stop
    | Some(a) => Return(a);
};

let rec contains = (query: tag => QueryResult.t(unit), l: t): bool => {
  let go = contains(query);
  switch (l.layout) {
  | Linebreak
  | Text(_) => false
  | Align(l) => go(l)
  | Cat(l1, l2) => go(l1) || go(l2)
  | Tagged(tag, l) =>
    switch (query(tag)) {
    | Stop => false
    | Skip => go(l)
    | Return () => true
    }
  };
};

let has_inline_OpenChild =
  contains(
    fun
    | Step(_)
    | DelimGroup => Skip
    | OpenChild({is_inline: true}) => Return()
    | _ => Stop,
  );

let has_para_OpenChild =
  contains(
    fun
    | Step(_)
    | DelimGroup => Skip
    | OpenChild({is_inline: false}) => Return()
    | _ => Stop,
  );

// TODO should be possible to make polymorphic over tag
// but was getting confusing type inference error
let rec find_and_decorate_Tagged =
        (decorate: (metrics, tag, t) => QueryResult.t(t), l: t): option(t) => {
  let go = find_and_decorate_Tagged(decorate);
  switch (l) {
  | {layout: Linebreak | Text(_), _} => None
  | {layout: Align(l1), _} =>
    go(l1) |> Opt.map(l1 => {...l, layout: Align(l1)})
  | {layout: Cat(l1, l2), _} =>
    switch (go(l1)) {
    | Some(l1) => Some({...l, layout: Cat(l1, l2)})
    | None => go(l2) |> Opt.map(l2 => {...l, layout: Cat(l1, l2)})
    }
  | {layout: Tagged(tag, l1), metrics} =>
    switch (decorate(metrics, tag, l1)) {
    | Stop => None
    | Skip =>
      go(l1) |> Opt.map(l1 => {Layout.metrics, layout: Tagged(tag, l1)})
    | Return(l) => Some(l)
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
    |> find_and_decorate_Tagged((metrics, tag: TermTag.t, l: t) => {
         switch (tag) {
         | Step(step) when step == next_step =>
           l
           |> go(~steps=rest)
           |> Opt.map(l => {Layout.metrics, layout: Tagged(tag, l)})
           |> QueryResult.of_opt
         | OpenChild(_)
         | ClosedChild(_)
         | DelimGroup
         | Term(_) => Skip
         | _ => Stop
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
           find_and_decorate_Tagged((metrics, tag, l) =>
             switch (tag) {
             | Text(text_data) =>
               Return({
                 metrics,
                 layout:
                   l
                   |> Layout.tag(
                        TermTag.Text({...text_data, caret: Some(j)}),
                      ),
               })
             | Term(_) => Skip
             | _ => Stop
             }
           )
         | OnOp(side) =>
           find_and_decorate_Tagged((metrics, tag, l) =>
             switch (tag) {
             | Op(op_data) =>
               Return({
                 metrics,
                 layout:
                   l
                   |> Layout.tag(
                        TermTag.Op({...op_data, caret: Some(side)}),
                      ),
               })
             | _ => Stop
             }
           )
         | OnDelim(k, side) =>
           find_and_decorate_Tagged((metrics, tag, l) =>
             switch (tag) {
             | Delim({path: (_, index), _} as delim_data) =>
               index == k
                 ? Return({
                     metrics,
                     layout:
                       l
                       |> Layout.tag(
                            TermTag.Delim({
                              ...delim_data,
                              caret: Some(side),
                            }),
                          ),
                   })
                 : Stop
             | Term(_)
             | DelimGroup => Skip
             | _ => Stop
             }
           )
         },
     );

// TODO document difference from follow_steps_and_decorate
let rec find_and_decorate_Term =
        (
          ~steps: CursorPath.steps,
          ~decorate_Term: (metrics, TermTag.term_data, t) => t,
          l: t,
        )
        : option(t) => {
  let go = find_and_decorate_Term(~decorate_Term);
  switch (steps) {
  | [] =>
    l
    |> find_and_decorate_Tagged((metrics, tag, l) =>
         switch (tag) {
         | Term(term_data) => Return(decorate_Term(metrics, term_data, l))
         | _ => Stop
         }
       )
  | [next_step, ...rest] =>
    l
    |> find_and_decorate_Tagged((metrics, tag, l) => {
         let take_step = () =>
           l
           |> go(~steps=rest)
           |> Opt.map(l => {Layout.metrics, layout: Tagged(tag, l)})
           |> QueryResult.of_opt;
         let found_term_if = (cond, term_data) =>
           cond && rest == []
             ? QueryResult.Return(decorate_Term(metrics, term_data, l))
             : Skip;
         switch (tag) {
         | Step(step) => step == next_step ? take_step() : Stop
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
         | Term({shape: Operand(_) | Rule, _}) => Skip
         | _ => Stop
         };
       })
  };
};

let find_and_decorate_cursor =
  find_and_decorate_Term(~decorate_Term=(metrics, term_data, l) =>
    {
      metrics,
      layout:
        l |> Layout.tag(TermTag.Term({...term_data, has_cursor: true})),
    }
  );
