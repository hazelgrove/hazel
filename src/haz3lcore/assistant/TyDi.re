open Util;
open OptUtil.Syntax;

//TODO(andrew): PERF DANGER!!
let z_to_ci = (~ctx: Ctx.t, z: Zipper.t) => {
  let map =
    z
    |> Zipper.smart_seg(~ignore_selection=true, ~dump_backpack=true)
    |> MakeTerm.go
    |> fst
    |> Statics.mk_map_ctx(ctx);
  let* index = Indicated.index(z);
  Id.Map.find_opt(index, map);
};

let unk: Typ.t = Unknown(Internal);

let expected_ty: Info.t => Typ.t =
  fun
  | InfoExp({mode, _})
  | InfoPat({mode, _}) => Typ.of_mode(mode)
  | _ => unk;

let const_mono_delim_tys: list((Token.t, Typ.t)) = [
  ("true", Bool),
  ("false", Bool),
  ("[]", List(unk)), //NOTE: would need to refactor buffer for this to show up
  ("()", Prod([])), //NOTE: would need to refactor buffer for this to show up
  ("\"\"", String), //NOTE: irrelavent as second quote appears automatically
  ("_", unk),
];

let leading_delim_tys: list((Token.t, Typ.t)) = [
  ("case ", unk),
  ("fun ", Arrow(unk, unk)),
  ("if ", unk),
  ("let ", unk),
  ("test ", Prod([])),
  ("type ", unk),
];

let infix_delim_tys: list((Token.t, Typ.t)) = [
  (",", unk), //NOTE: current approach doesn't work for this, but irrelevant as 1-char
  ("::", List(unk)),
  (";", unk),
  ("&&", Bool),
  ("||", Bool),
  ("$==", Bool),
  ("==.", Bool),
  ("==", Bool),
  ("<", Bool),
  (">", Bool),
  ("<=", Bool),
  (">=", Bool),
  ("<.", Bool),
  (">.", Bool),
  ("<=.", Bool),
  (">=.", Bool),
  ("+", Int),
  ("-", Int),
  ("*", Int),
  ("/", Int),
  ("**", Int),
  ("+.", Float),
  ("-.", Float),
  ("*.", Float),
  ("/.", Float),
  ("**.", Float),
];

let filter_by_type =
    (ctx: Ctx.t, expected_ty: Typ.t, self_tys: list((Token.t, Typ.t))) =>
  List.filter_map(delim => {
    let* self_ty = List.assoc_opt(delim, self_tys);
    let+ _ = Typ.join(ctx, expected_ty, self_ty);
    delim;
  });

let ctx_candidates = (ci: Info.t): list(string) => {
  let ctx = Info.ctx_of(ci);
  switch (ci) {
  | InfoExp({mode, _}) =>
    ctx |> Ctx.filtered_entries(~return_ty=true, Typ.of_mode(mode))
  | InfoPat({mode, _}) =>
    ctx |> Ctx.filtered_tag_entries(~return_ty=true, Typ.of_mode(mode))
  | InfoTyp(_) => Ctx.get_alias_names(ctx)
  | _ => []
  };
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

let delim_candidates = (ty_map, delims_of_sort, ci: Info.t): list(string) => {
  let sort = Info.sort_of(ci);
  let delims = delims_of_sort(sort);
  switch (sort) {
  | Exp
  | Pat => filter_by_type(Info.ctx_of(ci), expected_ty(ci), ty_map, delims)
  | _ => delims
  };
};

let candidates = (ci: Info.t, z: Zipper.t): list(string) => {
  backpack_candidate(Info.sort_of(ci), z)
  @ delim_candidates(leading_delim_tys, Molds.delayed_leading_delims, ci)
  @ delim_candidates(infix_delim_tys, Molds.infix_delims, ci)
  @ delim_candidates(const_mono_delim_tys, Molds.const_mono_delims, ci)
  @ ctx_candidates(ci);
};

let left_of_mono = (z: Zipper.t) =>
  switch (
    z.relatives.siblings |> fst |> List.rev,
    z.relatives.siblings |> snd,
  ) {
  | ([Tile({label: [tok_to_left], _}), ..._], _) => Some(tok_to_left)
  | _ => None
  };

let mk_pseudotile =
    (~sort: Sort.t, id_gen: Id.t, z: Zipper.t, t: Token.t): (Id.t, Tile.t) => {
  let (id, id_gen) = IdGen.fresh(id_gen);
  //NOTE: precedence is max so it will be tight to thing it's completing
  let nibs = Siblings.fit_of(~p=Precedence.max, ~sort, z.relatives.siblings);
  let mold: Mold.t = {out: sort, in_: [], nibs};
  (id_gen, {id, label: [t], shards: [0], children: [], mold});
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

let set_buffer =
    (~ctx: Ctx.t, z: Zipper.t, id_gen: Id.t): option((Zipper.t, Id.t)) => {
  let* tok_to_left = left_of_mono(z);
  let* ci = z_to_ci(~ctx, z);
  let candidates = candidates(ci, z);
  //print_endline("CANDIDATES:\n" ++ (candidates |> String.concat("\n")));
  // a filtered candidate is a prefix match with at least one more char
  //TODO(andrew): need to escape tok_to_left, e.g. dots....
  let filtered_candidates =
    candidates
    |> List.filter(String.starts_with(~prefix=tok_to_left))
    |> List.filter((!=)(tok_to_left));
  //print_endline("FILT:\n" ++ (filtered_candidates |> String.concat("\n")));
  let* top_candidate = filtered_candidates |> Util.ListUtil.hd_opt;
  let* candidate_suffix = suffix_of(top_candidate, tok_to_left);
  //print_endline("CANDIDATE: " ++ candidate_suffix);
  let (id, tile) =
    mk_pseudotile(~sort=Info.sort_of(ci), id_gen, z, candidate_suffix);
  let z = Zipper.set_buffer(z, ~content=[Tile(tile)], ~mode=Amorphous);
  Some((z, id));
};