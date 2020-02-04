[@deriving sexp]
type annot = TermAnnot.t;

[@deriving sexp]
type t = Layout.t(annot);

// TODO shouldn't need this, refactor to use option
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

let rec contains = (query: annot => QueryResult.t(unit), l: t): bool => {
  let go = contains(query);
  switch (l) {
  | Linebreak
  | Text(_) => false
  | Align(l) => go(l)
  | Cat(l1, l2) => go(l1) || go(l2)
  | Annot(annot, l) =>
    switch (query(annot)) {
    | Stop => false
    | Skip => go(l)
    | Return () => true
    }
  };
};

let has_child =
  contains(
    fun
    | OpenChild(_)
    | ClosedChild(_) => Return()
    | _ => Skip,
  );

let has_inline_OpenChild =
  contains(
    fun
    | Step(_)
    | DelimGroup
    | LetLine => Skip
    | OpenChild({is_inline: true}) => Return()
    | _ => Stop,
  );

let has_para_OpenChild =
  contains(
    fun
    | Step(_)
    | DelimGroup
    | LetLine => Skip
    | OpenChild({is_inline: false}) => Return()
    | _ => Stop,
  );

// TODO should be possible to make polymorphic over annot
// but was getting confusing type inference error
let rec find_and_decorate_Annot =
        (decorate: (annot, t) => QueryResult.t(t), l: t): option(t) => {
  let go = find_and_decorate_Annot(decorate);
  switch (l) {
  | Linebreak
  | Text(_) => None
  | Align(l1) => go(l1) |> OptUtil.map(l1 => Layout.Align(l1))
  | Cat(l1, l2) =>
    switch (go(l1)) {
    | Some(l1) => Some(Cat(l1, l2))
    | None => go(l2) |> OptUtil.map(l2 => Layout.Cat(l1, l2))
    }
  | Annot(annot, l1) =>
    switch (decorate(annot, l1)) {
    | Stop => None
    | Skip => go(l1) |> OptUtil.map(l1 => Layout.Annot(annot, l1))
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
    |> find_and_decorate_Annot((annot: TermAnnot.t, l: t) => {
         switch (annot) {
         | Step(step) when step == next_step =>
           l
           |> go(~steps=rest)
           |> OptUtil.map(l => Layout.Annot(annot, l))
           |> QueryResult.of_opt
         | OpenChild(_)
         | ClosedChild(_)
         | DelimGroup
         | LetLine
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
           find_and_decorate_Annot((annot, l) =>
             switch (annot) {
             | Text(text_data) =>
               Return(
                 l
                 |> Layout.annot(
                      TermAnnot.Text({...text_data, caret: Some(j)}),
                    ),
               )
             | EmptyLine
             | Term(_) => Skip
             | _ => Stop
             }
           )
         | OnOp(side) =>
           find_and_decorate_Annot((annot, l) =>
             switch (annot) {
             | Op(op_data) =>
               Return(
                 l
                 |> Layout.annot(
                      TermAnnot.Op({...op_data, caret: Some(side)}),
                    ),
               )
             | _ => Stop
             }
           )
         | OnDelim(k, side) =>
           find_and_decorate_Annot((annot, l) =>
             switch (annot) {
             | Delim({path: (_, index), _} as delim_data) =>
               index == k
                 ? Return(
                     l
                     |> Layout.annot(
                          TermAnnot.Delim({
                            ...delim_data,
                            caret: Some(side),
                          }),
                        ),
                   )
                 : Stop
             | Term(_)
             | DelimGroup
             | LetLine => Skip
             | _ => Stop
             }
           )
         },
     );

// TODO document difference from follow_steps_and_decorate
let rec find_and_decorate_Term =
        (
          ~steps: CursorPath.steps,
          ~decorate_Term: (TermAnnot.term_data, t) => t,
          l: t,
        )
        : option(t) => {
  let go = find_and_decorate_Term(~decorate_Term);
  switch (steps) {
  | [] =>
    l
    |> find_and_decorate_Annot((annot, l) =>
         switch (annot) {
         | Term(term_data) => Return(decorate_Term(term_data, l))
         | _ => Stop
         }
       )
  | [next_step, ...rest] =>
    l
    |> find_and_decorate_Annot((annot, l) => {
         let take_step = () =>
           l
           |> go(~steps=rest)
           |> OptUtil.map(l => Layout.Annot(annot, l))
           |> QueryResult.of_opt;
         let found_term_if = (cond, term_data) =>
           cond && rest == []
             ? QueryResult.Return(decorate_Term(term_data, l)) : Skip;
         switch (annot) {
         | Step(step) => step == next_step ? take_step() : Stop
         | Term({shape: SubBlock({hd_index, _}), _} as term_data) =>
           found_term_if(hd_index == next_step, term_data)
         | Term({shape: NTuple({comma_indices, _}), _} as term_data) =>
           found_term_if(comma_indices |> List.mem(next_step), term_data)
         | Term({shape: BinOp({op_index, _}), _} as term_data) =>
           found_term_if(op_index == next_step, term_data)
         | OpenChild(_)
         | ClosedChild(_)
         | DelimGroup
         | LetLine
         | Term({shape: Operand(_) | Case(_) | Rule, _}) => Skip
         | _ => Stop
         };
       })
  };
};

let find_and_decorate_cursor =
  find_and_decorate_Term(~decorate_Term=(term_data, l) =>
    l |> Layout.annot(TermAnnot.Term({...term_data, has_cursor: true}))
  );

let find_and_decorate_var_use =
  find_and_decorate_Term(~decorate_Term=(term_data, l) =>
    switch (term_data) {
    | {shape: Var(var_data), _} =>
      l
      |> Layout.annot(
           TermAnnot.Term({
             ...term_data,
             shape: Var({...var_data, show_use: true}),
           }),
         )
    | _ => failwith(__LOC__ ++ ": var not found")
    }
  );

module PathSearchResult = {
  type t =
    | Found(CursorPath.t)
    | Transport(Side.t)
    | NotFound;

  let of_opt =
    fun
    | None => NotFound
    | Some(path) => Found(path);

  let to_opt =
    fun
    | NotFound
    | Transport(_) => None
    | Found(path) => Some(path);
};

let path_before = (l: t): option(CursorPath.t) => {
  let rec go = (l: t): PathSearchResult.t =>
    switch (l) {
    | Text(_)
    | Linebreak => NotFound
    | Align(l) => go(l)
    | Cat(l1, l2) =>
      switch (go(l1)) {
      | (NotFound | Transport(Before) | Found(_)) as fin => fin
      | Transport(After) => go(l2)
      }
    | Annot(
        OpenChild(_) | ClosedChild(_) | DelimGroup | LetLine | EmptyLine |
        Step(_) |
        Term(_),
        l,
      ) =>
      go(l)
    | Annot(Padding | HoleLabel(_) | SpaceOp | UserNewline, _) => NotFound
    | Annot(Indent, _) => Transport(After)
    | Annot(Text({steps, _}), _) => Found((steps, OnText(0)))
    | Annot(Op({steps, _}), _) => Found((steps, OnOp(Before)))
    | Annot(Delim({path: (steps, k), _}), _) =>
      Found((steps, OnDelim(k, Before)))
    };
  go(l) |> PathSearchResult.to_opt;
};

let path_after = (l: t): option(CursorPath.t) => {
  let rec go = (l: t): PathSearchResult.t =>
    switch (l) {
    | Text(_)
    | Linebreak => NotFound
    | Align(l) => go(l)
    | Cat(l1, l2) =>
      switch (go(l2)) {
      | (NotFound | Transport(After) | Found(_)) as fin => fin
      | Transport(Before) => go(l1)
      }
    | Annot(
        OpenChild(_) | ClosedChild(_) | DelimGroup | LetLine | EmptyLine |
        Step(_) |
        Term(_),
        l,
      ) =>
      go(l)
    | Annot(UserNewline, _) => Transport(Before)
    | Annot(Padding | HoleLabel(_) | SpaceOp | Indent, _) => NotFound
    | Annot(Text({steps, length, _}), _) => Found((steps, OnText(length)))
    | Annot(Op({steps, _}), _) => Found((steps, OnOp(After)))
    | Annot(Delim({path: (steps, k), _}), _) =>
      Found((steps, OnDelim(k, After)))
    };
  go(l) |> PathSearchResult.to_opt;
};

let path_of_caret_position = (row: int, col: int, l: t): option(CursorPath.t) => {
  let rec go = (indent, current_row, current_col, l: t): PathSearchResult.t => {
    let metrics = Layout.metrics(l);
    let end_row = current_row + metrics.height - 1;
    let end_col =
      (metrics.height == 1 ? current_col : indent) + metrics.last_width;
    if (current_row == end_row && col <= indent) {
      PathSearchResult.of_opt(path_before(l));
    } else if (current_row == end_row && col >= end_col) {
      PathSearchResult.of_opt(path_after(l));
    } else {
      // current_row != end_row || indent < col < end_col
      let leaning_side: Side.t =
        col - current_col <= end_col - col ? Before : After;
      switch (l) {
      | Text(_)
      | Annot(HoleLabel(_), _) => NotFound
      | Linebreak => Transport(row == end_row ? After : Before)
      | Align(l) => l |> go(current_col, current_row, current_col)
      | Cat(l1, l2) =>
        let metrics1 = Layout.metrics(l1);
        let mid_row = current_row + metrics1.height - 1;
        let mid_col =
          (metrics1.height == 1 ? current_col : indent) + metrics1.last_width;
        if (row == mid_row && col == mid_col) {
          switch (path_after(l1), path_before(l2)) {
          | (None, None) => NotFound
          | (Some(path), _)
          | (_, Some(path)) => Found(path)
          };
        } else if (row < mid_row || row == mid_row && col < mid_col) {
          switch (l1 |> go(indent, current_row, current_col)) {
          | (NotFound | Transport(Before) | Found(_)) as fin => fin
          | Transport(After) => PathSearchResult.of_opt(path_before(l2))
          };
        } else {
          // row >= mid_row && (row != mid_row || col > mid_col)
          switch (l2 |> go(indent, mid_row, mid_col)) {
          | (NotFound | Transport(After) | Found(_)) as fin => fin
          | Transport(Before) => PathSearchResult.of_opt(path_after(l1))
          };
        };
      | Annot(
          OpenChild(_) | ClosedChild(_) | DelimGroup | LetLine | EmptyLine |
          Step(_) |
          Term(_),
          l,
        ) =>
        l |> go(indent, current_row, current_col)
      | Annot(UserNewline, _) => Transport(Before)
      | Annot(Indent, _) => Transport(After)
      | Annot(Padding | SpaceOp, _) => Transport(leaning_side)
      | Annot(Text({steps, _}), _) =>
        Found((steps, OnText(col - current_col)))
      | Annot(Op({steps, _}), _) => Found((steps, OnOp(leaning_side)))
      | Annot(Delim({path: (steps, k), _}), _) =>
        Found((steps, OnDelim(k, leaning_side)))
      };
    };
  };
  l |> go(0, 0, 0) |> PathSearchResult.to_opt;
};
