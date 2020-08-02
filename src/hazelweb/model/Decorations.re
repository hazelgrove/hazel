open Sexplib.Std;

[@deriving sexp]
type t = {
  err_holes: list(CursorPath_common.steps),
  var_err_holes: list(CursorPath_common.steps),
  var_uses: list(CursorPath_common.steps),
  current_term: option(CursorPath_common.t),
};

/*

 0 let x = 1 in
 1 let y = 2 in
 2 x + y
 3 -------------
   result is 3
   -------------
 4 x + y
 5 -------------
   result is 3
   -------------

 issues to consider for full design
 - currently green cursor indicator on line items
   extends all the way to the bottom of the program.
   does this mean we need to split the program into
   parts and restrict decorations to the parts?
   - another issue is cursor indicators have particular
     meanings w.r.t. open child borders, should they
     extend past cell boundaries to respect that meaning?
     should we rethink cells / cell boundaries so that
     a concluding expression is not required, and redesign
     cursor indicators for this context?
 - cell boundaries should be able to be arbitrary height,
   in which case the offset origin for positioning a decoration
   within a cell should be the top left corner of the cell,
   not the top left corner of the overall program
   - maybe what we do with multiline livelits is what we want here
 - if we decide to split the program into parts and decorate
   only one part at a time, that doesn't actually work for, say,
   variable usage
 */

let is_empty = (ds: t): bool =>
  ListUtil.is_empty(ds.err_holes)
  && ListUtil.is_empty(ds.var_err_holes)
  && ListUtil.is_empty(ds.var_uses)
  && ds.current_term == None;

let take_step = (step: int, decorations: t): t => {
  let {err_holes, var_err_holes, current_term, var_uses} = decorations;
  let remove_step =
    fun
    | [step', ...steps] when step == step' => Some(steps)
    | _ => None;
  let err_holes = err_holes |> List.filter_map(remove_step);
  let var_err_holes = var_err_holes |> List.filter_map(remove_step);
  let var_uses = var_uses |> List.filter_map(remove_step);
  let current_term =
    Option.bind(current_term, ((steps, cursor)) =>
      remove_step(steps) |> Option.map(steps => (steps, cursor))
    );
  {err_holes, var_err_holes, var_uses, current_term};
};

let current =
    (term_sort: TermSort.t, term_shape: TermShape.t, decorations: t)
    : list(Decoration.t) => {
  let is_current = steps =>
    switch (term_shape) {
    | SubBlock({hd_index, _}) => steps == [hd_index]
    | NTuple({comma_indices, _}) =>
      List.exists(n => steps == [n], comma_indices)
    | BinOp({op_index, _}) => steps == [op_index]
    | Operand
    | Case
    | Rule => steps == []
    };
  let err_holes =
    decorations.err_holes
    |> List.find_opt(is_current)
    |> Option.map(_ => Decoration.ErrHole)
    |> Option.to_list;
  let var_err_holes =
    decorations.var_err_holes
    |> List.find_opt(is_current)
    |> Option.map(_ => Decoration.VarErrHole)
    |> Option.to_list;
  let var_uses =
    decorations.var_uses
    |> List.find_opt(is_current)
    |> Option.map(_ => Decoration.VarUse)
    |> Option.to_list;
  let current_term =
    switch (decorations.current_term) {
    | Some((steps, _)) when is_current(steps) => [
        Decoration.CurrentTerm(term_sort, term_shape),
      ]
    | _ => []
    };
  List.concat([err_holes, var_err_holes, var_uses, current_term]);
};
