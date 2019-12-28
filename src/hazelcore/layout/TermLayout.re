open GeneralUtil;

[@deriving sexp]
type tag = TermTag.t;

[@deriving sexp]
type t = Layout.t(tag);

module QueryResult = {
  type t('a) =
    | Fail
    | Skip
    | Return('a);

  let of_opt: option('a) => t('a) =
    fun
    | None => Fail
    | Some(a) => Return(a);
};

let rec contains = (query: tag => QueryResult.t(unit), l: t): bool => {
  let go = contains(query);
  switch (l) {
  | Linebreak
  | Text(_) => false
  | Align(l) => l |> go
  | Cat(l1, l2) => go(l1) || go(l2)
  | Tagged(tag, l) =>
    switch (tag |> query) {
    | Fail => false
    | Skip => l |> go
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
    | _ => Fail,
  );

let has_para_OpenChild =
  contains(
    fun
    | Step(_)
    | DelimGroup => Skip
    | OpenChild({is_inline: false}) => Return()
    | _ => Fail,
  );

// TODO should be possible to make polymorphic over tag
// but was getting confusing type inference error
let rec find_and_decorate_Tagged =
        (decorate: (tag, t) => QueryResult.t(t), l: t): option(t) => {
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
    | Fail => None
    | Skip => l |> go |> Opt.map(Layout.tag(tag))
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
    |> find_and_decorate_Tagged((tag: TermTag.t, l: t) => {
         switch (tag) {
         | Step(step) when step == next_step =>
           l
           |> go(~steps=rest)
           |> Opt.map(Layout.tag(tag))
           |> QueryResult.of_opt
         | OpenChild(_)
         | ClosedChild(_)
         | DelimGroup
         | Term(_) => Skip
         | _ => Fail
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
               Return(
                 l
                 |> Layout.tag(TermTag.Text({...text_data, caret: Some(j)})),
               )
             | Term(_) => Skip
             | _ => Fail
             }
           )
         | OnOp(side) =>
           find_and_decorate_Tagged((tag, l) =>
             switch (tag) {
             | Op(op_data) =>
               Return(
                 l
                 |> Layout.tag(TermTag.Op({...op_data, caret: Some(side)})),
               )
             | _ => Fail
             }
           )
         | OnDelim(k, side) =>
           find_and_decorate_Tagged((tag, l) =>
             switch (tag) {
             | Delim({path: (_, index), _} as delim_data) =>
               index == k
                 ? Return(
                     l
                     |> Layout.tag(
                          TermTag.Delim({...delim_data, caret: Some(side)}),
                        ),
                   )
                 : Fail
             | Term(_)
             | DelimGroup => Skip
             | _ => Fail
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
         | Term(term_data) => Return(decorate_Term(term_data, l))
         | _ => Fail
         }
       )
  | [next_step, ...rest] =>
    l
    |> find_and_decorate_Tagged((tag, l) => {
         let take_step = () =>
           l
           |> go(~steps=rest)
           |> Opt.map(Layout.tag(tag))
           |> QueryResult.of_opt;
         let found_term_if = (cond, term_data) =>
           cond && rest == []
             ? QueryResult.Return(decorate_Term(term_data, l)) : Skip;
         switch (tag) {
         | Step(step) => step == next_step ? take_step() : Fail
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
         | _ => Fail
         };
       })
  };
};

let find_and_decorate_cursor =
  find_and_decorate_Term(~decorate_Term=(term_data, l) =>
    l |> Layout.tag(TermTag.Term({...term_data, has_cursor: true}))
  );
