open Util;
open OptUtil.Syntax;
//DONE: when flattening zipper for semantics, disregard emphemeral selection
//DONE: inspect shapes on both sides, use those instead, sorts too
//TODO: on accept, trigger merge somehow
//TODO: only activate when caret pos = Outer
/*
 init version: exp/pat only, convex mono only, ctx only (no forms)
 check if last of l_sib is convex mono
 get ci, check if exp/pat. get ctx.
 filter ctx by expected ty
 get ctx names. search names by prefix regexp, get hd.
 on accept:
 check syntactic shit (convex mono to right, right-chev mono only in sel, form a valid token if combined)
 if so, blank selection (redundant?) and paste text content of chev

  */

//TODO(andrew): PERF DANGER!!

let z_to_ci = (z: Zipper.t) => {
  let map =
    z
    |> Zipper.unselect_and_zip(~ignore_selection=true)
    |> MakeTerm.go
    |> fst
    |> Statics.mk_map;
  let* index = Indicated.index(z);
  Id.Map.find_opt(index, map);
};

let candidates = (ci: Info.t): list(string) =>
  switch (ci) {
  | InfoExp({ctx, mode: Ana(ty), _}) => ctx |> Ctx.filtered_entries(ty)
  | InfoExp({ctx, mode: Syn | SynFun, _}) =>
    ctx |> Ctx.filtered_entries(Unknown(Internal))
  | InfoPat({ctx, mode: Ana(ty), _}) => ctx |> Ctx.filtered_entries(ty)
  | InfoPat({ctx, mode: Syn | SynFun, _}) =>
    ctx |> Ctx.filtered_entries(Unknown(Internal))
  | InfoTyp({ctx, _}) => Ctx.get_alias_names(ctx)
  | _ => []
  };

/* Criteria: selection is ephemeral and a single monotile with the caret on the left,
   and the left sibling ends in a monotile, such that appending the two would result
   in a valid token */
let complete_criteria = (z: Zipper.t) =>
  switch (
    z.selection.focus,
    z.selection.ephemeral,
    z.selection.content,
    z.relatives.siblings |> fst |> List.rev,
    z.relatives.siblings |> snd,
  ) {
  | (
      Left,
      true,
      [Tile({label: [completion], _})],
      [Tile({label: [tok_to_left], _}), ..._],
      _,
    )
      when Form.is_valid_token(tok_to_left ++ completion) =>
    Some(completion)
  | _ => None
  };

let left_of_mono = (z: Zipper.t) =>
  switch (
    z.relatives.siblings |> fst |> List.rev,
    z.relatives.siblings |> snd,
  ) {
  | ([Tile({label: [tok_to_left], _}), ..._], _) => Some(tok_to_left)
  | _ => None
  };

let mk_pseudotile = (id_gen: Id.t, z: Zipper.t, t: Token.t): (Id.t, Tile.t) => {
  let (id, id_gen) = IdGen.fresh(id_gen);
  let nibs = Siblings.fit_of(z.relatives.siblings);
  let mold: Mold.t = {out: Any, in_: [], nibs};
  //TODO(andrew): better sort than Any
  (id_gen, {id, label: [t], shards: [0], children: [], mold});
};

let add_ephemeral_selection = (z: Zipper.t, tile): Zipper.t => {
  ...z,
  selection: {
    ...z.selection,
    ephemeral: true,
    content: [Tile(tile)],
  },
};

let suffix_of = (candidate: Token.t, left: Token.t): option(Token.t) => {
  let candidate_suffix =
    String.sub(
      candidate,
      String.length(left),
      String.length(candidate) - String.length(left),
    );
  candidate_suffix == "" ? None : Some(candidate_suffix);
};

let mk_pseudoselection =
    (z: Zipper.t, id_gen: Id.t): option((Zipper.t, Id.t)) => {
  let* tok_to_left = left_of_mono(z);
  let* ci = z_to_ci(z);
  let candidates = candidates(ci);
  //print_endline("CANDIDATES:\n" ++ (candidates |> String.concat("\n")));
  // a filtered candidate is a prefix match with at least one more char
  let filtered_candidates =
    candidates |> List.filter(Form.regexp("^" ++ tok_to_left ++ "."));
  //print_endline("FILT:\n" ++ (filtered_candidates |> String.concat("\n")));
  let* top_candidate = filtered_candidates |> Util.ListUtil.hd_opt;
  let* candidate_suffix = suffix_of(top_candidate, tok_to_left);
  //print_endline("CANDIDATE: " ++ candidate_suffix);
  let (id, tile) = mk_pseudotile(id_gen, z, candidate_suffix);
  let z = add_ephemeral_selection(z, tile);
  Some((z, id));
};
