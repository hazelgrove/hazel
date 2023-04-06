open Util;
open OptUtil.Syntax;

/*
   idea: if there are ~ no current suggestions, and there's a type error(?),
   suggest a following infix to address it, e.g. on "let a:Float = fst(1.0|"
   suggest comma, or on "let b:Bool = 1|" suggest <, >, <=, >=, ==, !=, etc.

  idea: maybe more likely to use case/if if you have unused variables

  IDEA: add a keybinding to reveal suggestion, even where one might not otherwise show?
     could use this to give prefix suggestions e.g... actually, do we want this...

  how to decide if a literal is likely vs variable vs computation

  heuristic: dual-informed:
  in app funpos: favor input ty consistent with arg
  in case scrut, favor pattern ty (or tys if incosistent?)
  in pat ann typpos: favor patann expected type

  heuristic: if type inconsistent, and no suggestions otherwise,
  suggest inserting infix after that would make it consistent

  heuristic: deprioritize fns returning unknown & variables of type unknown

  2-multihole: pretend it's an ap?
     choose funpos based on parent expect and other branch
     (if to non-prefix, A><B| choose arg similarly, but wrap it in parens A><(B|) )

  A|><B
  2-multihole, alternate: pretend its another binop
     get self tys of both branches, filter based on Statics.typ_exp_binop

 2-multihole, alternate alternate: suggest applicable bp drop if possible

  stylistic: in funpos: suggest only vars/tags/aps-producing-arrows

  */

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
  //below is an attempt to support [],(); not sure why it doesnt work (causes exception blah)
  /* this doesnt work because fit_of actually needs more info to determine nib shape
     if this isnt a monotile; we would need to return the label here as well to figure
     out the fit */
  | ([Tile({label: [tok_to_left, ..._], shards: [0], _}), ..._], _) =>
    print_endline("left_of_mono: " ++ tok_to_left ++ "");
    Some(tok_to_left);
  | _ => None
  };

let mk_amorphous_tile =
    (~sort: Sort.t, id_gen: Id.t, sibs: Siblings.t, t: Token.t)
    : (Id.t, Tile.t) => {
  let (id, id_gen) = IdGen.fresh(id_gen);
  let mold = Siblings.mold_fitting_between(sort, Precedence.max, sibs);
  (id_gen, {id, label: [t], shards: [0], children: [], mold});
};

let get_amorphous_buffer_text = (z: Zipper.t): option(Token.t) =>
  switch (z.selection.mode, z.selection.content) {
  | (Buffer(Amorphous), [Tile({label: [completion], _})]) =>
    Some(completion)
  | _ => None
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
  let filtered_candidates =
    candidates
    |> List.filter(String.starts_with(~prefix=tok_to_left))
    |> List.filter((!=)(tok_to_left));
  //print_endline("FILT:\n" ++ (filtered_candidates |> String.concat("\n")));
  let* top_candidate = filtered_candidates |> Util.ListUtil.hd_opt;
  let* candidate_suffix = suffix_of(top_candidate, tok_to_left);
  //print_endline("CANDIDATE: " ++ candidate_suffix);
  let sort = Info.sort_of(ci);
  let (id, tile) =
    mk_amorphous_tile(~sort, id_gen, z.relatives.siblings, candidate_suffix);
  let z = Zipper.set_buffer(z, ~content=[Tile(tile)], ~mode=Amorphous);
  Some((z, id));
};
