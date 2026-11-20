open Util;

/* The part of a term a selection names, and the pieces that draw it.
 *
 * The part is the smallest node holding the selection or, in a chain of
 * an associative operator, the run of operands the selection touches
 * (`Language.Occurrence.of_selection`): selecting `2 + 3`, or just `+ 3`,
 * in `1 + 2 + 3` names the run `2 + 3`, which the parser leaves without
 * a node. The ids of `term` are those of the selection's pieces. */

let of_zipper =
    (
      ~measured: Measured.t,
      ~term_data: TermData.t,
      ~term: Language.Exp.t,
      z: Zipper.t,
    )
    : option(Language.Occurrence.t) => {
  open OptUtil.Syntax;
  let selection = z.selection.content;
  let* id = TermData.get_root_id_using_ranges(selection, term_data, measured);
  let+ node = Language.Exp.find_by_id(id, term);
  Language.Occurrence.of_selection(
    ~ids=Segment.IDs.all(selection),
    ~goal=term,
    node,
  );
};

/* A node's own pieces, or the range of its chain's pieces that holds the
 * run's operands. Parentheses around part of a chain are one piece, so a
 * run that ends inside them takes in all of them. */
let segment =
    (~term_data: TermData.t, part: Language.Occurrence.t): option(Segment.t) =>
  switch (part) {
  | Node(node) => TermData.segment(Language.Exp.rep_id(node), term_data)
  | Run({op, chain, operands, _}) =>
    open OptUtil.Syntax;
    let rec bare = (e: Language.Exp.t) =>
      switch (Language.Exp.term_of(e)) {
      | Parens(inner) when Language.Occurrence.Chain.is(op, inner) =>
        bare(inner)
      | _ => e
      };
    let rec ids_in = (e: Language.Exp.t) =>
      Language.IdTagged.ids(e)
      @ List.concat_map(Language.Occurrence.children(e), ~f=ids_in);
    let ids =
      List.concat_map(
        ~f=(o: Language.Occurrence.Chain.operand) => ids_in(o.exp),
        operands,
      );
    let+ seg =
      TermData.segment(Language.Exp.rep_id(bare(chain)), term_data);
    let in_run =
      List.mapi(~f=(i, piece) => (i, piece), seg)
      |> List.filter(~f=((_, piece)) =>
           List.exists(Segment.IDs.all_piece(piece), ~f=id =>
             List.mem(ids, id, ~equal=Id.equal)
           )
         )
      |> List.map(~f=fst);
    switch (in_run) {
    | [] => seg
    | [i, ...is] =>
      ListUtil.sublist(
        (
          List.fold(is, ~init=i, ~f=min),
          List.fold(is, ~init=i, ~f=max) + 1,
        ),
        seg,
      )
    };
  };

/* The selection expanded to the part it names, as an editor that expands
 * its selection draws it. A node `term` doesn't have keeps its own
 * pieces. */
let expanded =
    (
      ~measured: Measured.t,
      ~term_data: TermData.t,
      ~term: Language.Exp.t,
      z: Zipper.t,
    )
    : option(Segment.t) =>
  switch (of_zipper(~measured, ~term_data, ~term, z)) {
  | Some(part) => segment(~term_data, part)
  | None =>
    TermData.get_root_id_using_ranges(
      z.selection.content,
      term_data,
      measured,
    )
    |> Option.bind(_, ~f=id => TermData.segment(id, term_data))
  };
