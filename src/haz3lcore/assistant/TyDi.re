open Util;
open OptUtil.Syntax;

/*

   IDEA: Add a keybinding to force reveal suggestion if not current shown.
   I've stubbed this out (Cmd+?) but needs an option to show suggestions
   even if on hole (ie prefix for completion is "")

   IDEA: If there are ~ no current suggestions, and the indicated term
   has a type error suggest following infixes which fix that type error,
   e.g. given "let a:Float = fst(1.0|" suggest comma
   e.g. given "let b:Bool = 1|" suggest <, >, <=, >=, ==, !=, etc.

   IDEA: UNBIDIRECTIONAL POSITIONS:
  1. In ap funpos: favor input ty consistent with arg
  2. In case scrut, favor the tys of extant patterns
  3. In list element, favor the tys of extant elements
  3. In pattern annotation type: favor patann expected type

  IDEA: If on infix op, suggest based on either operand type,
  especially the case where it would fix an operand type error

  IDEA: If on 2-multihole, suggest infix ops as above or Ap if applicable

 */

//TODO(andrew): PERF DANGER!!
let z_to_ci = (~settings: CoreSettings.t, ~ctx: Ctx.t, z: Zipper.t) => {
  let map =
    z
    |> MakeTerm.from_zip_for_sem
    |> fst
    |> Interface.Statics.mk_map_ctx(settings, ctx);
  let* index = Indicated.index(z);
  Id.Map.find_opt(index, map);
};

let unk: Typ.t = Unknown(Internal);

let expected_ty: Info.t => Typ.t =
  fun
  | InfoExp({mode, _})
  | InfoPat({mode, _}) => Mode.ty_of(mode)
  | _ => unk;

let const_mono_delim_tys: list((Token.t, Typ.t)) = [
  ("true", Bool),
  ("false", Bool),
  //("[]", List(unk)), / *NOTE: would need to refactor buffer for this to show up */
  //("()", Prod([])), /* NOTE: would need to refactor buffer for this to show up */
  ("\"\"", String), /* NOTE: Irrelevent as second quote appears automatically */
  ("_", unk) //TODO(andrew): make sure only shows in pattern pos...
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
  (",", unk), /* NOTE: Current approach doesn't work for this, but irrelevant as 1-char */
  ("::", List(unk)),
  ("@", List(unk)),
  (";", unk),
  ("&&", Bool),
  ("\\/", Bool),
  ("$==", Bool),
  ("==.", Bool),
  ("==", Bool),
  ("!=", Bool),
  ("!=.", Bool),
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
  ("++", String),
];

let filter_by_type =
    (ctx: Ctx.t, expected_ty: Typ.t, self_tys: list((Token.t, Typ.t))) =>
  List.filter_map(delim => {
    let* self_ty = List.assoc_opt(delim, self_tys);
    Typ.is_consistent(ctx, expected_ty, self_ty) ? Some(delim) : None;
  });

let co_ctx_candidates = (ctx: Ctx.t, co_ctx: CoCtx.t): list(string) => {
  List.filter_map(
    ((name, _)) =>
      switch (Ctx.lookup_var(ctx, name)) {
      | None => Some(name)
      | Some(_) => None
      },
    co_ctx,
  );
};

let _completion_nontrivially_consistent =
    (ctx: Ctx.t, ty_expect: Typ.t, ty_given: Typ.t): bool =>
  switch (ty_expect, ty_given) {
  | (_, Unknown(_)) => false
  | _ => Typ.is_consistent(ctx, ty_expect, ty_given)
  };

let filtered_entries = (ty_expect: Typ.t, ctx: Ctx.t): list(string) =>
  /* get names of all var entries consistent with ty */
  List.filter_map(
    fun
    | Ctx.VarEntry({typ: Arrow(_, ty_out) as ty_arr, name, _})
        when
          Typ.is_consistent(ctx, ty_expect, ty_out)
          && !Typ.is_consistent(ctx, ty_expect, ty_arr) => {
        Some
          (name ++ "("); // TODO(andrew): this is a hack
      }
    | VarEntry({typ, name, _}) when Typ.is_consistent(ctx, ty_expect, typ) =>
      Some(name)
    | _ => None,
    ctx,
  );

let filtered_ctr_entries = (ty: Typ.t, ctx: Ctx.t): list(string) =>
  /* get names of all constructor entries consistent with ty */
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({typ: Arrow(_, ty_out) as ty_arr, name, _})
        when
          Typ.is_consistent(ctx, ty, ty_out)
          && !Typ.is_consistent(ctx, ty, ty_arr) =>
      Some(name ++ "(") // TODO(andrew): this is a hack
    | ConstructorEntry({typ, name, _}) when Typ.is_consistent(ctx, ty, typ) =>
      Some(name)
    | _ => None,
    ctx,
  );

let ctx_candidates = (ci: Info.t): list(string) => {
  let ctx = Info.ctx_of(ci);
  switch (ci) {
  | InfoExp({mode, _}) =>
    filtered_entries(Mode.ty_of(mode), ctx)
    @ filtered_ctr_entries(Mode.ty_of(mode), ctx)
  | InfoPat({mode, co_ctx, _}) =>
    filtered_ctr_entries(Mode.ty_of(mode), ctx)
    @ co_ctx_candidates(ctx, co_ctx)
  | InfoTyp(_) => Ctx.get_alias_names(ctx)
  | _ => []
  };
};

let backpack_candidate = (_sort: Sort.t, z: Zipper.t) => {
  /* NOTE: Disabled sort check for now. Needs to be more
     subtle to get stuff like -> completion for "fun x -|"
     (sort is Pat, mold.out is Exp).  */
  switch (z.backpack) {
  | [] => []
  | [{content, _}, ..._] =>
    switch (content) {
    | [Tile({label, shards: [idx], mold: _, _})]
        when Zipper.can_put_down(z) /*&& sort == mold.out*/ => [
        List.nth(label, idx),
      ]
    | _ => []
    }
  };
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
    Some(tok_to_left)
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

let candidates = (ci: Info.t, z: Zipper.t): list(string) => {
  backpack_candidate(Info.sort_of(ci), z)
  /* NOTE: Sorting here ensures that if we have an exact match already,
     we won't suggest extending it, but sorting may not be desirable in
     other ways, for example maybe we want recency bias in ctx?
     Possibly revisit this.

     I'm sorting here as opposed to after combination because I always
     want backpack candidates to show up first  */
  @ (
    delim_candidates(leading_delim_tys, Molds.delayed_leading_delims, ci)
    @ delim_candidates(const_mono_delim_tys, Molds.const_mono_delims, ci)
    @ ctx_candidates(ci)
    |> List.sort(String.compare)
  )
  @ (
    delim_candidates(infix_delim_tys, Molds.infix_delims, ci)
    |> List.sort(String.compare)
  );
};

let set_buffer =
    (~settings, ~ctx: Ctx.t, z: Zipper.t, id_gen: Id.t)
    : option((Zipper.t, Id.t)) => {
  let* tok_to_left = left_of_mono(z);
  let* ci = z_to_ci(~settings, ~ctx, z);
  let candidates = candidates(ci, z);
  let filtered_candidates =
    candidates |> List.filter(String.starts_with(~prefix=tok_to_left));
  let* top_candidate = filtered_candidates |> Util.ListUtil.hd_opt;
  let* candidate_suffix = suffix_of(top_candidate, tok_to_left);
  let (id, tile) =
    mk_amorphous_tile(
      ~sort=Info.sort_of(ci),
      id_gen,
      z.relatives.siblings,
      candidate_suffix,
    );
  let z = Zipper.set_buffer(z, ~content=[Tile(tile)], ~mode=Amorphous);
  Some((z, id));
};
