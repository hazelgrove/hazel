open Util;
open OptUtil.Syntax;

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

let ctx_candidates = (ci: Info.t): list(string) =>
  //TODO: also suggest things with arrow type whose return type meets expectation
  switch (ci) {
  | InfoExp({ctx, mode: Ana(ty), _}) =>
    ctx |> Ctx.filtered_entries(~return_ty=true, ty)
  | InfoExp({ctx, mode: Syn | SynFun, _}) =>
    ctx |> Ctx.filtered_entries(Unknown(Internal))
  | InfoPat({ctx, mode: Ana(ty), _}) =>
    ctx |> Ctx.filtered_tag_entries(~return_ty=true, ty)
  | InfoPat({ctx, mode: Syn | SynFun, _}) =>
    ctx |> Ctx.filtered_tag_entries(Unknown(Internal))
  | InfoTyp({ctx, _}) => Ctx.get_alias_names(ctx) @ Form.base_typs
  | _ => []
  };

let backpack_candidate = (sort: Sort.t, z: Zipper.t) =>
  switch (z.backpack) {
  | [] => []
  | [{content, _}, ..._] =>
    switch (content) {
    | [Tile({label, shards: [idx], mold, _})] when sort == mold.out => [
        List.nth(label, idx),
      ]
    | _ => []
    }
  };

let const_candidates = (ci: Info.t): list(string) =>
  switch (ci) {
  | InfoExp({mode: Ana(Bool | Unknown(_)) | Syn | SynFun, _}) => Form.bools
  | InfoPat({mode: Ana(Bool | Unknown(_)) | Syn | SynFun, _}) => Form.bools
  | InfoTyp(_) => Form.base_typs
  | _ => []
  };

let candidates = (z: Zipper.t): option(list(string)) => {
  let+ ci = z_to_ci(z);
  let sort = Info.sort_of(ci);
  backpack_candidate(sort, z)
  @ ctx_candidates(ci)
  @ const_candidates(ci)
  @ Molds.leading_delims(sort);
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
      when
        Form.is_valid_token(tok_to_left ++ completion)
        || String.sub(completion, String.length(completion) - 1, 1) == "(" =>
    //TODO(andrew): second clause is hack see Ctx.re filtered_entries
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
  let* candidates = candidates(z);
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
