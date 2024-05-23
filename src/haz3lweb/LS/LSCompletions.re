open Util;
open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  init_ctx: Ctx.t,
  completions: LSActions.completions,
  data: LSActions.data,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type completability =
  | Completeable(Id.t, string)
  | Inert(Id.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type left_shape =
  | LeftConvex
  | LeftConcave;

[@deriving (show({with_path: false}), sexp, yojson)]
type maybe_padded =
  | Just
  | SpacePadded;

[@deriving (show({with_path: false}), sexp, yojson)]
type left_neighbor_info = option((maybe_padded, left_shape, completability));

[@deriving (show({with_path: false}), sexp, yojson)]
type right_neighbor_info =
  | Nothing
  | ConvexHole(Id.t)
  | StringLit(string)
  | Comment(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type incompleteness =
  | Grammatical
  | Contextual
  | Fine;

[@deriving (show({with_path: false}), sexp, yojson)]
type infodump = {
  ci: Info.t,
  bidi_ci: Info.t,
  bidi_ctx_cls: Term.Cls.t,
  bidi_ctx_expected_ty: Typ.t,
  bidi_parent_ci: option(Info.t),
  bidi_parent_ctx_cls: option(Term.Cls.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type generation_options =
  | OnlyCompletion(infodump, string)
  | OnlyCompletionString(string)
  | OnlyCompletionComment(string)
  | NewRightConvex(infodump)
  | CompletionOrNewRightConvex(infodump, string, infodump) //TODO: betterize
  | NewRightConcave(infodump)
  | CompletionOrNewRightConcave(infodump, string);

[@deriving (show({with_path: false}), sexp, yojson)]
type pre_grammar = {
  completions: list(string),
  new_tokens: list(string),
};

/* Assume for now left-to-right entry, so the present shards are
   a prefix of the complete tile. this means that regardless of
   completeness, we can just use the left nibs */

let left_nib_dir = (t: Tile.t): Direction.t =>
  fst(t.mold.nibs).shape == Convex ? Left : Right;

let right_nib_dir = (t: Tile.t): Direction.t =>
  Tile.is_complete(t)
    ? snd(t.mold.nibs).shape == Convex ? Right : Left : Left;

let right_nib_str = (t: Tile.t): string =>
  right_nib_dir(t) == Left ? "<" : ">";

let left_nib_str = (t: Tile.t): string =>
  left_nib_dir(t) == Left ? "<" : ">";

let tile_str = (t: Tile.t): string => {
  let label_str = t.label |> String.concat(" ");
  left_nib_str(t) ++ label_str ++ right_nib_str(t);
};

let thing_to_right = (~db, z: Zipper.t): right_neighbor_info =>
  /* If we're doing pure left to right entry, there should be nothing to
     the right except for maybe a convex hole inserted by the grouter.
     If there is a such a hole, its CI should be used to inform new
     token insertions (but NOT completions of the token to the left) */
  switch (z.relatives.siblings |> snd) {
  | [] =>
    db("  LSP: Syntax: No rightwards piece");
    Nothing;
  | [Grout({id, shape: Convex, _})] =>
    /* If the leftward neighbor has a rightwards concave nib,
       a convex grout will be inserted to the right of the caret.
       We record its ID as its CI can be used to inform
       new token insertions */
    db("  LSP: Syntax: Rightwards piece is Convex Grout");
    ConvexHole(id);
  | [Tile({label: [str], _})] when Form.is_string(str) =>
    /* Special case: When we insert a quote, another quote is inserted
       to the right of the caret. This allow string literals, which are
       implemented as a single token, to 'emulate' the ghost mechanics,
       even though quotes are not proper delimiters. This case is the
       only known situation where the caret position may be Inner during
       pure left-to-right entry */
    StringLit(str)
  | [Secondary({content: Comment(str), _}), ..._] =>
    /* Special case: As above. Note that we allow an optional thing after it,
       as there may be grout after a comment */
    Comment(str)
  | [_, ..._] as rhs =>
    db("  LSP: Syntax: Rightwards segment is: " ++ Segment.show(rhs));
    failwith(
      "  LSP: Syntax: EXN: Nonempty Rightwards segment not single Convex Grout or String literal or Comment",
    );
  };

let is_completable = (t: Tile.t) =>
  switch (t.shards |> List.map(List.nth(t.label))) {
  | _ when List.length(t.label) == 1 =>
    /* Monotiles are completable */
    Completeable(t.id, List.hd(t.label))
  | [("(" | "[") as tok_to_left] =>
    /* SPECIAL CASE: Instant-expanding leading polytiles
       that also have a monotile completion */
    Completeable(t.id, tok_to_left)
  | _ =>
    /* Other polytiles are not completable */
    Inert(t.id)
  };

let piece_to_left = (~db, z: Zipper.t): left_neighbor_info =>
  /*
   Returning Left means we're looking for either a new thing
    that starts with a left-facing chevron, or a completion of the
    thing to the left, which has a left-side right-facing chevron.
    */
  switch (z.relatives.siblings |> fst |> List.rev) {
  | [] => None
  | [lht, ..._] as seg =>
    if (Piece.is_secondary(lht)) {
      db("  LSP: Syntax: Leftward is Secondary: trimming");
    };
    switch (Segment.trim_secondary(Left, seg)) {
    | [] => failwith("  LSP: Syntax: EXN: Rightwards seg empty after trim")
    | [lht', ..._] =>
      switch (lht') {
      | Tile(t) =>
        db(
          "  LSP: Syntax: Leftward is "
          ++ (Tile.is_complete(t) ? "Complete" : "Incomplete")
          ++ " Tile: "
          ++ tile_str(t),
        );
        let c = is_completable(t);
        let d = right_nib_dir(t) == Right ? LeftConvex : LeftConcave;
        let p = Piece.is_secondary(lht) ? SpacePadded : Just;
        Some((p, d, c));
      | Grout({id: _, shape, _}) =>
        failwith(
          "  LSP: Syntax: EXN: Leftward Grout " ++ Grout.show_shape(shape),
        )
      | Secondary(_) =>
        failwith("  LSP: Syntax: EXN: Secondary after trimming secondaries")
      }
    };
  };

let of_sugs: list(Suggestion.t) => string =
  s => s |> List.map((s: Suggestion.t) => s.content) |> String.concat(" ");

let error_str = ci =>
  switch (Info.error_of(ci)) {
  | Some(err) => Info.show_error(err)
  | None => "None"
  };

let unk = Typ.Unknown(Internal);

let expected_exp = (ci: Info.exp): Typ.t =>
  Typ.normalize(ci.ctx, Mode.ty_of(ci.mode));

let expected_pat = (ci: Info.pat): Typ.t =>
  Typ.normalize(ci.ctx, Mode.ty_of(ci.mode));

let expected_ty = (ci: Info.t): Typ.t =>
  switch (ci) {
  | InfoExp(exp) => expected_exp(exp)
  | InfoPat(pat) => expected_pat(pat)
  | _ => unk
  };

let self_exp = (ci: Info.exp): Typ.t =>
  switch (Self.typ_of_exp(ci.ctx, ci.self)) {
  | Some(actual) => Typ.normalize(ci.ctx, actual)
  | None => unk
  };

let self_pat = (ci: Info.pat): Typ.t =>
  switch (Self.typ_of_pat(ci.ctx, ci.self)) {
  | Some(actual) => Typ.normalize(ci.ctx, actual)
  | None => unk
  };

let self_ty = (ci: Info.t): Typ.t =>
  switch (ci) {
  | InfoExp(exp) => self_exp(exp)
  | InfoPat(pat) => self_pat(pat)
  | _ => unk
  };

let is_incomplete = (ci: Info.t): incompleteness =>
  /* Is the term (which should be a monotile) something that
     is invalid on its own, but might become valid by extension?
     NOTE: keyword expansion isn't explictly taken into account
     here, since keywords are suggested with trailing spaces,
     so they'll always have a completion suggestion even when
     the keyword is fully written. If this is changed, this will
     need to change as well. */
  switch (ci) {
  | InfoExp({status: InHole(Common(NoType(BadToken(_) | MultiError))), _})
  | InfoPat({status: InHole(Common(NoType(BadToken(_) | MultiError))), _})
  /* Note: an example of the multierror case is "1:" in an expression context */
  | InfoTyp({status: InHole(BadToken(_)), _}) => Grammatical
  | InfoExp({status: InHole(FreeVariable(_)), _})
  | InfoExp({status: InHole(Common(NoType(FreeConstructor(_)))), _})
  | InfoPat({status: InHole(Common(NoType(FreeConstructor(_)))), _})
  | InfoTyp({status: InHole(FreeTypeVariable(_)), _}) => Contextual
  | InfoTyp({status: InHole(DuplicateConstructor(_)), _})
  | InfoTPat({status: InHole(ShadowsType(_)), _}) =>
    /* Kind of an abuse but whatever */
    Contextual
  | _ => Fine
  };

let show_info =
    (db, info_map, ci: Info.t, bidi_ci, bidi_parent_ci, z: Zipper.t) => {
  let sort = Info.sort_of(ci);
  let cls = Info.cls_of(ci);
  let ctx = Info.ctx_of(ci);
  let expected_ty = AssistantForms.Typ.expected(ci);
  let backpack_tokens = AssistantBackpack.to_token_list(z.backpack);
  let errors = Haz3lcore.ErrorPrint.collect_static(info_map);
  db("  LSP: Info: Cls: " ++ Term.Cls.show(cls));
  db("  LSP: Info: Sort: " ++ Sort.to_string(sort));
  db("  LSP: Info: Expected type: " ++ Typ.to_string(expected_ty));
  db("  LSP: Info: Seft type: " ++ Typ.to_string(self_ty(ci)));
  db("  LSP: Info: Error Status: " ++ error_str(ci));
  db("  LSP: Info: Typing Context: " ++ Ctx.to_string(ctx));
  db("  LSP: Info: Backpack stack: " ++ String.concat(" ", backpack_tokens));
  if (errors != []) {
    db("  LSP: Info: ALL errors:\n" ++ String.concat("  \n", errors));
  };
  let expected_ty = AssistantForms.Typ.expected(bidi_ci);
  db("  LSP: Info: BidiCtx: Cls: " ++ Term.Cls.show(Info.cls_of(bidi_ci)));
  db("  LSP: Info: BidiCtx: Expected type: " ++ Typ.to_string(expected_ty));
  switch (bidi_parent_ci) {
  | Some(ci) =>
    db("  LSP: Info: Bidi Parent Cls: " ++ Term.Cls.show(Info.cls_of(ci)))
  | None => db("  LSP: Info: Bidi Parent: Root")
  };
};

let print_gen_option = (~db, gen_options: generation_options): unit =>
  switch (gen_options) {
  | NewRightConvex(_id) => db("  LSP: Syntax: Can insert left-convex")
  | NewRightConcave(_id) => db("  LSP: Syntax: Can insert left-concave")
  | CompletionOrNewRightConvex(_id_l, tok_to_left, _id_new) =>
    db("  LSP: Syntax: Can insert left-convex or complete: " ++ tok_to_left)
  | CompletionOrNewRightConcave(_id_l, tok_to_left) =>
    db("  LSP: Syntax: Can insert left-concave or complete: " ++ tok_to_left)
  | OnlyCompletionString(stringlit) =>
    db("LSP: Must extend/complete stringlit: " ++ stringlit)
  | OnlyCompletionComment(comment) =>
    db("LSP: Must extend/complete comment: " ++ comment)
  | OnlyCompletion(_id, tok_to_left) =>
    db("  LSP: Syntax: Must complete: " ++ tok_to_left)
  };

let get_bidi_id = (z: Zipper.t, indicated_id: Id.t) => {
  let orig_segment =
    Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true, z);
  let map = Measured.path_map(orig_segment);
  let old_path = Id.Map.find(indicated_id, map); //TODO: catch or opt
  let new_z = Zipper.zip_to_path(orig_segment, old_path, Outer);
  let seg: Segment.t = new_z.relatives.siblings |> Siblings.zip;
  seg
  |> Segment.skel
  |> Skel.root
  |> Aba.map_a(List.nth(seg))
  |> Aba.first_a
  |> Piece.id;
};

let get_info_map = (~init_ctx, z: Zipper.t) =>
  z
  |> MakeTerm.from_zip_for_sem
  |> fst
  |> Interface.Statics.mk_map_ctx(CoreSettings.on, init_ctx);

let term_of = (~db, s: option(string)) => {
  let s =
    switch (s) {
    | None => ""
    | Some(s) => s
    };
  let z = LSFiles.process_zipper(~db, s);
  let seg = Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true, z);
  fst(MakeTerm.go(seg));
};

let get_prelude_ctx =
    (~db, ~init_ctx, ~prelude: option(string), ~common: option(string))
    : Ctx.t => {
  let term_common = term_of(~db, common);
  let term_prelude = term_of(~db, prelude);
  let probe = 13371338;
  let probe_term = Term.UExp.{ids: [Id.mk()], term: Int(probe)};
  let term =
    EditorUtil.append_exp(
      term_common,
      EditorUtil.append_exp(term_prelude, probe_term),
    );
  let info_map =
    Interface.Statics.mk_map_ctx(CoreSettings.on, init_ctx, term);
  let get_sym = (_, info: Info.t, acc) =>
    switch (info) {
    | InfoExp({ctx, term, _}) when term == probe_term => Some(ctx)
    | _ => acc
    };
  switch (Id.Map.fold(get_sym, info_map, None)) {
  | Some(ctx) => ctx
  | None =>
    failwith(
      "LSP: Prelude: EXN: Couldn't find sym to extract ctx: "
      ++ string_of_int(probe),
    )
  };
};

let collate_info = (~init_ctx, ~db, z: Zipper.t, id: Id.t): infodump => {
  let info_map = get_info_map(~init_ctx, z);
  let get_info = id => Id.Map.find_opt(id, info_map);
  let ci =
    OptUtil.get_or_fail(
      "LSP: Gen: EXN: Couldn't find CI for id " ++ Id.to_string(id),
      get_info(id),
    );
  let bidi_ci =
    OptUtil.get_or_fail(
      "LSP: Gen: EXN: Couldn't find Bidi CI for id " ++ Id.to_string(id),
      get_info(get_bidi_id(z, id)),
    );
  let bidi_parent_ci =
    bidi_ci
    |> Info.ancestors_of
    |> ListUtil.hd_opt
    |> OptUtil.and_then(get_info);
  /* TODO: Disabled any_errors at the moment as I think it's too restrictive.
      For example, given "let x:Bool = (1" we want to suggest ")".
     Closer is gating on errors in bidelimited ctx, but even that
     doesn't work in above, as error is on the 1. Parens may be
     special case here.

     Must be no errors to drop:
     "let = in" (inner types independent of expectation)
     "type = in" (inner things independent)
     "fun ->" (inner type independent)
     "| =>" (inner type independent of expectation)
     "test end" (since inner type independent of expected type)

     Errors might be ok:
     "()" (parens) e.g. given "let a:Bool = (1" could do ") < 1"
     "case end" e.g. given "let a:Bool = case 1 | 1 => 1" could do "end < 1"

     UNSURE:
     "<???>(<???>)" (fn ap; might be okay since binds v tight)
     "[<???>]"
     "if <fine> then <???> else"

     Logic is maybe something like:
     Can drop, if resultant operand after dropping only has errors
     due to expectation for which there is a resolvant type path.

     Or maybe: it shouldnt be about checking for errors per se, but
     instead analyzing the contents of the bidelimited context against
     a type with a typepath to /its/ bidelimited ctx

     */
  show_info(db, info_map, ci, bidi_ci, bidi_parent_ci, z);
  {
    ci,
    bidi_ci,
    bidi_ctx_cls: Info.cls_of(bidi_ci),
    bidi_ctx_expected_ty: expected_ty(bidi_ci),
    bidi_parent_ci,
    bidi_parent_ctx_cls: Option.map(Info.cls_of, bidi_parent_ci),
  };
};

let generation_options =
    (~db, ~init_ctx, ~completions: LSActions.completions, z: Zipper.t) => {
  let get_info = collate_info(~db, ~init_ctx, z);
  switch (piece_to_left(~db, z), thing_to_right(~db, z)) {
  | (None, Nothing) => failwith("LSP: EXN: Nothing to left or right")
  | (Some((to_left, shape, compl)), Nothing) =>
    switch (shape) {
    | LeftConcave =>
      failwith("LSP: EXN: Concave to left and nothing to right")
    | LeftConvex =>
      switch (to_left, compl) {
      | (Just, Inert(id_l))
      | (SpacePadded, Completeable(id_l, _) | Inert(id_l)) =>
        NewRightConcave(get_info(id_l))
      | (Just, Completeable(id_l, left_token)) =>
        let left_info = get_info(id_l);
        switch (is_incomplete(left_info.ci)) {
        | Grammatical =>
          db("  LSP: Syntax: Bad token; only completion");
          OnlyCompletion(left_info, left_token);
        | Contextual when completions == Types || completions == Context =>
          db("  LSP: Syntax: Free token; only completion");
          OnlyCompletion(left_info, left_token);
        | _ => CompletionOrNewRightConcave(left_info, left_token)
        };
      }
    }
  | (None, ConvexHole(id)) => NewRightConvex(get_info(id))
  | (Some((to_left, shape, compl)), ConvexHole(id_r)) =>
    switch (shape) {
    | LeftConvex => failwith("LSP: EXN: Convex to left and right")
    | LeftConcave =>
      switch (to_left, compl) {
      | (Just, Inert(_))
      | (SpacePadded, _) => NewRightConvex(get_info(id_r))
      | (Just, Completeable(id_l, left_tok)) =>
        let left_info = get_info(id_l);
        switch (is_incomplete(get_info(id_l).ci)) {
        | Grammatical =>
          db("  LSP: Syntax: Bad token; only completion");
          OnlyCompletion(left_info, left_tok);
        | Contextual when completions == Types || completions == Context =>
          db("  LSP: Syntax: Free token; only completion");
          OnlyCompletion(left_info, left_tok);
        | _ =>
          CompletionOrNewRightConvex(left_info, left_tok, get_info(id_r))
        };
      }
    }
  | (_, StringLit(id)) => OnlyCompletionString(id)
  | (_, Comment(id)) => OnlyCompletionComment(id)
  };
};

let get_backpack_sugs = (~convex, z: Zipper.t): Suggestion.s =>
  List.filter_map(
    convex ? AssistantBackpack.is_convex : AssistantBackpack.is_concave,
    AssistantBackpack.suggest(z),
  );

let get_lookahead_tys = (~db, ctx: Ctx.t, ty: Typ.t): list(Typ.t) => {
  let ty = Typ.normalize(ctx, ty);
  db("LS: Lookahead: Generating lookaheads for: " ++ Typ.show(ty));
  let ty_paths = AssistantCtx.get_lookahead_tys_exp(ctx, ty);
  db("LS: Lookahead: Paths:\n " ++ AssistantCtx.show_type_path(ty_paths));
  let tys =
    List.map(Util.ListUtil.last, ty_paths) |> List.sort_uniq(compare);
  db(
    "LS: Lookahead: Tys:\n " ++ String.concat("\n ", List.map(Typ.show, tys)),
  );
  tys;
};

let rec is_strict_prefix_up_to_consistency = (ctx, p_syn, p_ana) =>
  switch (p_syn, p_ana) {
  | (_, []) => false
  | ([], _) => true
  | ([ty_syn, ...p_syn], [ty_ana, ...p_ana]) =>
    Typ.is_consistent(ctx, ty_syn, ty_ana)
    && is_strict_prefix_up_to_consistency(ctx, p_syn, p_ana)
  };

let suggest_comma_inner = (~db, ~ctx: Ctx.t, ~self: Typ.t, expected: Typ.t) =>
  /* Assumes self and expected types are normalized */
  switch (expected) {
  | Unknown(_) => true
  | Prod([Prod(_), ..._]) =>
    //TODO(andrew): complete hack
    true
  | Prod(p_ana) =>
    db(
      "LSP: commas: p_ana is prod: "
      ++ String.concat(" ", List.map(Typ.show, p_ana)),
    );
    db("LSP: commas: self type is " ++ Typ.show(self));
    switch (p_ana, self) {
    | (_, Unknown(_)) => true // technically redundant?
    | (_, Prod(p_syn)) =>
      is_strict_prefix_up_to_consistency(ctx, p_syn, p_ana)
    | ([t1_ana, ..._], _) =>
      //TODO(andrew): figure out what's up with consistency
      //calls to join with any_type,unknown(typehole) are false??
      //patched that here but wtf
      Typ.is_consistent(ctx, t1_ana, self)
    | _ => false
    };
  | _ => false
  };

let suggest_comma = (~db, bidi_ctx_ci: Info.t) => {
  db("LS: Suggest comma generator active");
  switch (bidi_ctx_ci) {
  | InfoExp(exp) =>
    db("LS: Suggest comma Exp case");
    db("LS: Suggest comma Exp case: typ: " ++ Typ.show(expected_exp(exp)));
    db("LS: Suggest comma Exp case: self: " ++ Typ.show(self_exp(exp)));
    let (expected, self) = (expected_exp(exp), self_exp(exp));
    expected
    |> get_lookahead_tys(~db, exp.ctx)
    |> List.exists(suggest_comma_inner(~db, ~ctx=exp.ctx, ~self));
  | InfoPat(pat) =>
    let (expected, self) = (expected_pat(pat), self_pat(pat));
    expected
    |> get_lookahead_tys(~db, pat.ctx)
    |> List.exists(suggest_comma_inner(~db, ~ctx=pat.ctx, ~self));
  | InfoTyp(_) => true
  | InfoTPat(_) => false
  };
};
// let suggest_comma = (~db, bidi_ctx_ci: Info.t) => {
//   db("LS: Suggest comma generator active");
//   switch (bidi_ctx_ci) {
//   | InfoExp({mode: SynFun, _})
//   | InfoPat({mode: SynFun, _}) => false
//   | InfoExp({mode: Syn | Ana(Unknown(_)), _})
//   | InfoPat({mode: Syn | Ana(Unknown(_)), _}) =>
//     //TODO(andrew): do we need to capture and normalize the ana type here?
//     true
//   | InfoExp({mode: Ana(ana), self: Common(Just(syn)), ctx, _})
//   | InfoPat({mode: Ana(ana), self: Common(Just(syn)), ctx, _}) =>
//     db("LS: Suggest comma Ana Just case");
//     db("LS: Suggest comma. Ana: " ++ Typ.show(ana));
//     db("LS: Suggest comma. Self: " ++ Typ.show(syn));
//     let ana = Typ.normalize(ctx, ana);
//     let self = Typ.normalize(ctx, syn);
//     let tys = get_lookahead_tys(~db, ctx, ana);
//     List.exists(suggest_comma_inner(~db, ~ctx, ~self), tys);
//   | InfoExp({mode: Ana(_), self: Free(_) | Common(_), _})
//   | InfoPat({mode: Ana(_), self: Common(_), _}) => false
//   | InfoTyp(_) => true
//   | InfoTPat(_) => false
//   };
// };

let n_ary_sugs = (~settings: settings, ~db, bidi_ci): Suggestion.s => {
  let comma_sug = Suggestion.mk(",");
  switch (settings.completions) {
  | Types => suggest_comma(~db, bidi_ci) ? [comma_sug] : []
  | Context
  | Grammar => [comma_sug]
  };
};

let dedup = List.sort_uniq(compare);

let suggest_exp = (~fns, ctx: Ctx.t, ty): Suggestion.s =>
  AssistantCtx.suggest_bound_exp(~fns, ty, ctx)
  @ AssistantForms.suggest_all_ty_convex(Exp, ctx, ty);

let suggest_pat = (~fns, ctx: Ctx.t, co_ctx, ty): Suggestion.s =>
  AssistantCtx.suggest_free_var(ty, ctx, co_ctx)
  @ AssistantCtx.suggest_bound_pat(~fns, ty, ctx)
  @ AssistantForms.suggest_all_ty_convex(Pat, ctx, ty);

let suggest_typ = (ctx: Ctx.t): Suggestion.s =>
  AssistantCtx.suggest_bound_typ(ctx)
  @ AssistantForms.suggest_all_ty_convex(Typ, ctx, unk);

let suggest_tpat = (ctx: Ctx.t): Suggestion.s =>
  AssistantForms.suggest_all_ty_convex(TPat, ctx, unk);

let convex_sugs = (~settings: settings, ci: Info.t) =>
  switch (settings.completions) {
  | Types =>
    switch (ci) {
    | InfoExp(exp) => suggest_exp(~fns=false, exp.ctx, expected_exp(exp))
    | InfoPat(pat) =>
      suggest_pat(~fns=false, pat.ctx, pat.co_ctx, expected_pat(pat))
    | InfoTyp({ctx, _}) => suggest_typ(ctx)
    | InfoTPat({ctx, _}) => suggest_tpat(ctx)
    }
  | Context =>
    switch (ci) {
    | InfoExp({ctx, _}) => suggest_exp(~fns=false, ctx, unk)
    | InfoPat({ctx, co_ctx, _}) => suggest_pat(~fns=false, ctx, co_ctx, unk)
    | InfoTyp({ctx, _}) => suggest_typ(ctx)
    | InfoTPat({ctx, _}) => suggest_tpat(ctx)
    }
  | Grammar =>
    switch (ci) {
    | InfoExp(_) =>
      [Suggestion.mk("~PATVAR~")]
      @ [Suggestion.mk("~CONSTRUCTOR~")]
      @ suggest_exp(~fns=false, [], unk)
    | InfoPat(_) =>
      [Suggestion.mk("~CONSTRUCTOR~")]
      @ suggest_pat(~fns=false, [], [], unk)
    | InfoTyp(_) => [Suggestion.mk("~TYPVAR~")] @ suggest_typ([])
    | InfoTPat(_) => suggest_tpat([])
    }
  };

let convex_lookahead_sugs = (~settings: settings, ~db, ci: Info.t) => {
  switch (settings.completions) {
  | Context
  | Grammar => []
  | Types =>
    switch (ci) {
    | InfoExp(exp) =>
      let ty = expected_exp(exp);
      let tys = get_lookahead_tys(~db, exp.ctx, ty);
      db(
        "  LS: Convex: Target types: "
        ++ (List.map(Typ.to_string, tys) |> String.concat(", ")),
      );
      suggest_exp(~fns=true, exp.ctx, ty)
      @ (List.map(suggest_exp(~fns=true, exp.ctx), tys) |> List.flatten);
    | InfoPat(pat) =>
      let ty = expected_pat(pat);
      let tys = AssistantCtx.get_lookahead_tys_pat(pat.ctx, ty);
      suggest_pat(~fns=true, pat.ctx, pat.co_ctx, ty)
      @ (
        List.map(suggest_pat(~fns=true, pat.ctx, pat.co_ctx), tys)
        |> List.flatten
      );
    | InfoTyp(_) => []
    | InfoTPat(_) => []
    }
  };
};

let postfix_sugs =
    (
      ~settings: settings,
      ~db as _,
      ci: Info.t,
      bidi_ctx_cls: Term.Cls.t,
      bidi_parent_ctx_cls: option(Term.Cls.t),
    )
    : Suggestion.s => {
  /* NOTE: We have to check both the bidictx and the parent here
     because the way case is implemented, rule tiles get the ci for
     the whole case, so if we're on a rule tile, the bidictx will be
     the whole case. (TODO: This is confusing, clarify) */
  let case_rule_sug =
    bidi_parent_ctx_cls == Some(Exp(Match)) || bidi_ctx_cls == Exp(Match)
      ? [Suggestion.mk("|")] : [];
  let postfix_ap_sug = (ctx, self_ty) =>
    /* Could alternatively make this more restrictive and require
       that actually arrow type not merely consistent. This would
       enforce use with appropriate constructor in patterns,
       but would screw up current impl of grammar/context-only generation */
    if (Typ.is_consistent(ctx, self_ty, Arrow(unk, unk))) {
      [Suggestion.mk("(")];
    } else {
      [];
    };
  switch (settings.completions) {
  | Grammar =>
    switch (ci) {
    | InfoExp(_) => postfix_ap_sug([], unk) @ case_rule_sug
    | InfoPat(_) => postfix_ap_sug([], unk)
    | InfoTyp(_) =>
      //TODO: make more ap more restrictive?
      [Suggestion.mk("(")]
    | InfoTPat(_) => []
    }
  | Context =>
    let ctx = Info.ctx_of(ci);
    switch (ci) {
    | InfoExp(_) => postfix_ap_sug(ctx, unk) @ case_rule_sug
    | InfoPat(_) => postfix_ap_sug(ctx, unk)
    | InfoTyp(_) =>
      //TODO: make more ap more restrictive?
      [Suggestion.mk("(")]
    | InfoTPat(_) => []
    };
  | Types =>
    switch (ci) {
    | InfoExp({ctx, _}) => postfix_ap_sug(ctx, self_ty(ci)) @ case_rule_sug
    | InfoPat({ctx, _}) => postfix_ap_sug(ctx, self_ty(ci))
    | InfoTyp(_) =>
      //TODO: make more ap more restrictive?
      [Suggestion.mk("(")]
    | InfoTPat(_) => []
    }
  };
};

let of_ops = (ctx, expected_ty: Typ.t, child_ty: Typ.t, ty1, ty2, ops) =>
  if (Typ.is_consistent(ctx, expected_ty, ty1)
      && Typ.is_consistent(ctx, child_ty, ty2)) {
    List.map(Suggestion.mk, ops);
  } else {
    [];
  };

let sug_exp_infix = (ctx: Ctx.t, l_child_ty: Typ.t, expected_ty: Typ.t) => {
  let of_ops = of_ops(ctx, expected_ty, l_child_ty);
  let bb = of_ops(Bool, Bool, ["&&", "\\/"]);
  let bs = of_ops(Bool, String, ["$=="]);
  let bu = of_ops(Bool, unk, ["=="]);
  let bi = of_ops(Bool, Int, ["!=", "<=", ">=", "<", ">"]);
  let bf = of_ops(Bool, Float, ["==.", "!=.", "<=.", ">=.", "<.", ">."]);
  let i = of_ops(Int, Int, ["+", "-", "*", "/", "**"]);
  let f = of_ops(Float, Float, ["+.", "-.", "*.", "/.", "**."]);
  let s = of_ops(String, String, ["++"]);
  let l1 = of_ops(List(unk), Typ.matched_list(ctx, expected_ty), ["::"]);
  /*Note: Using List(matched) in 2nd arg below because don't want this
    check to pass if expected is unknown but l_child is e.g. Int */
  let l2 =
    of_ops(List(unk), List(Typ.matched_list(ctx, expected_ty)), ["@"]);
  bb @ bs @ bi @ bu @ bf @ i @ f @ s @ l1 @ l2;
};

let infix_sugs =
    (
      ~completion: bool,
      ~settings: settings,
      ~db,
      ci: Info.t,
      bidi_ctx_expected_ty: Typ.t,
    )
    : Suggestion.s => {
  let infix_mono_sugs = AssistantForms.suggest_infix_mono(ci);
  switch (settings.completions) {
  | Grammar =>
    switch (ci) {
    | InfoExp(_) => sug_exp_infix([], unk, unk)
    | InfoPat(_)
    | InfoTyp(_)
    | InfoTPat(_) => infix_mono_sugs
    }
  | Context =>
    switch (ci) {
    | InfoExp({ctx, _}) => sug_exp_infix(ctx, unk, unk)
    | InfoPat(_)
    | InfoTyp(_)
    | InfoTPat(_) => infix_mono_sugs
    }
  | Types =>
    switch (ci) {
    | InfoExp(exp) =>
      /* 1. Calc skel of lseg and thread it here.
            2. recurse on skel, looking for infix operators
           3. if infix op lower precedence, rec on its left child.
             if left child is not lower precdence, cur will be parent of new op,
             and left child will be child of new op.
         4. in other words, want find furthest-down infix with lower precedence.
         5. then can generated expected_tys for type paths to the parent expected type
         6. and child_ty from child
         7. so basic fn will take precdence and seg, and return ids of prospective parent, child
          */
      //TODO: lookahead expected_tys should take precedence into account as well
      let expected_ty = expected_exp(exp);
      /* TODO: 'completion' condition  is a hack that gives too-liberal recommendations
         really need to figure out, for each operator completion, what
         the left child would actually become, and get that type. */
      let self_ty = completion ? unk : self_exp(exp);
      db(
        "  LSP: Concave: Infix: Left child Self type: "
        ++ Typ.to_string(self_ty),
      );
      let base = sug_exp_infix(exp.ctx, self_ty, expected_ty);
      db("  LSP: Concave: Infix: Base: " ++ of_sugs(base));
      let tys = get_lookahead_tys(~db, exp.ctx, bidi_ctx_expected_ty);
      let lookahead =
        List.map(sug_exp_infix(exp.ctx, self_ty), tys) |> List.flatten;
      db(
        "  LSP: Concave: Infix: Lookahead: " ++ of_sugs(lookahead |> dedup),
      );
      base @ lookahead;
    //TODO: get self_ty of actual prospective child, on a per-operator basis
    | InfoPat(_)
    | InfoTyp(_)
    | InfoTPat(_) => infix_mono_sugs
    }
  };
};

let completion_filter =
    (caret: Zipper.Caret.t, tok_to_left: string, sug: Suggestion.t) => {
  let sug = Suggestion.content_of(sug);
  /*  For fixed tokens, a suggestion is a valid completion if
      the token to the left of the caret is a proper prefix of the
      suggestion. If the suggestion is a regexp form like atomic
      literals or binding variable names, if the token to the left
      is a prefix of something matching the regexp, we want to
      suggest a regexp which represents possible ways of extending
      the token such that it remains a prefix of something matching
      the regexp.

       I assume this can be done generally for at least basic
      regular expressions, but for now each form is special-cased */
  let is_prefix_of_intlit = Form.is_int;
  let is_prefix_of_floatlit = t => Form.is_float(t) || Form.is_int(t);
  let is_prefix_if_stringlit = Form.is_string;
  let is_prefix_of_var = Form.is_var;
  let is_prefix_of_typvar = Form.is_typ_var;
  let is_prefix_of_ctr = Form.is_ctr;
  if (String.starts_with(~prefix=tok_to_left, sug)) {
    TyDi.suffix_of(sug, tok_to_left);
  } else {
    switch (sug) {
    | "~INTLIT~" when is_prefix_of_intlit(tok_to_left) =>
      /* TODO: limit length? */
      Some("~EXTEND-INTLIT~")
    | "~FLOATLIT~" when is_prefix_of_floatlit(tok_to_left) =>
      /* TODO: figure out general case incl NaN, length limit, etc. */
      Some("~EXTEND-FLOATLIT~")
    | "~STRINGLIT~" when is_prefix_if_stringlit(tok_to_left) =>
      /* HACK: This only actually covers the case of a finished stringlit
         (caret is Outer). The in-progress stringlit case is captured above
         (the StringLit case for thing_to_right. However we retain the
         currently-unreachable Some case below defenfsively. */
      caret == Outer ? None : Some("~EXTEND-STRINGLIT~")
    | "~PATVAR~" when is_prefix_of_var(tok_to_left) =>
      Some("~EXTEND-PATVAR~")
    | "~TYPVAR~" when is_prefix_of_typvar(tok_to_left) =>
      Some("~EXTEND-TYPVAR~")
    | "~CONSTRUCTOR~" when is_prefix_of_ctr(tok_to_left) =>
      Some("~EXTEND-CONSTRUCTOR~")
    | _ => None
    };
  };
};

let left_convex_sugs = (~settings, ~info_dump, ~db, z) => {
  let {ci, _} = info_dump;
  let left_convex_backpack_sugs = get_backpack_sugs(~convex=true, z);
  let convex_sugs = convex_sugs(~settings, ci);
  let convex_lookahead_sugs = convex_lookahead_sugs(~settings, ~db, ci);
  //TODO: should prefix be factored out here somewhere?
  db("  LSP: Convex: Backpack: " ++ of_sugs(left_convex_backpack_sugs));
  db("  LSP: Convex: Base: " ++ of_sugs(convex_sugs));
  db(
    "  LSP: Convex: Lookahead: " ++ of_sugs(convex_lookahead_sugs |> dedup),
  );
  left_convex_backpack_sugs
  @ convex_sugs
  @ convex_lookahead_sugs
  |> List.sort_uniq(Suggestion.compare);
};

let left_concave_sugs = (~info_dump, ~completion, ~settings, ~db, z) => {
  let {
    ci,
    bidi_ci,
    bidi_parent_ctx_cls,
    bidi_ctx_cls,
    bidi_ctx_expected_ty,
    _,
  } = info_dump;
  let left_concave_backpack_sugs = get_backpack_sugs(~convex=false, z);
  let infix_sugs =
    infix_sugs(~completion, ~settings, ~db, ci, bidi_ctx_expected_ty)
    |> List.sort_uniq(Suggestion.compare);
  let postfix_sugs =
    postfix_sugs(~settings, ~db, ci, bidi_ctx_cls, bidi_parent_ctx_cls);
  let n_ary_sugs = n_ary_sugs(~settings, ~db, bidi_ci);
  db("  LSP: Concave: Backpack: " ++ of_sugs(left_concave_backpack_sugs));
  db("  LSP: Concave: N-ary: " ++ of_sugs(n_ary_sugs));
  db("  LSP: Concave: Infix: " ++ of_sugs(infix_sugs));
  db("  LSP: Concave: Postfix: " ++ of_sugs(postfix_sugs));
  left_concave_backpack_sugs
  @ n_ary_sugs
  @ infix_sugs
  @ postfix_sugs
  |> List.sort_uniq(Suggestion.compare);
};

let mk_completions =
    (~settings: settings, ~db, ~tok_to_left: string, z: Zipper.t, info_dump)
    : list(string) => {
  /* Note that for completion suggestions, we ignore shape expectations,
     as the left tile might get keyword-completed or remolded */
  db("LSP: Generating Completions for prefix: " ++ tok_to_left);
  left_convex_sugs(~settings, ~db, ~info_dump, z)
  @ left_concave_sugs(~completion=true, ~settings, ~info_dump, ~db, z)
  |> List.filter_map(completion_filter(z.caret, tok_to_left))
  |> List.sort_uniq(String.compare);
};

let mk_new_left_convex =
    (~settings: settings, ~db, z: Zipper.t, info_dump): list(string) => {
  db("LSP: Generating new left convex tokens");
  left_convex_sugs(~settings, ~info_dump, ~db, z)
  |> List.map(Suggestion.content_of)
  |> List.sort_uniq(String.compare);
};

let mk_new_left_concave =
    (~settings: settings, ~db, z: Zipper.t, info_dump): list(string) => {
  db("LSP: Generating new left concave tokens");
  left_concave_sugs(~completion=false, ~settings, ~db, ~info_dump, z)
  |> List.map(Suggestion.content_of)
  |> List.sort_uniq(String.compare);
};

let dispatch_generation = (~settings: settings, ~db, z: Zipper.t): pre_grammar => {
  let seg_before = z.relatives.siblings |> fst |> List.rev;
  let seg_after = z.relatives.siblings |> snd;
  if (seg_before == [] && seg_after == []) {
    failwith("LSP: EXN: Empty segment");
  };
  let init_ctx =
    get_prelude_ctx(
      ~db,
      ~init_ctx=settings.init_ctx,
      ~common=settings.data.common,
      ~prelude=settings.data.prelude,
    );
  let gen_options =
    generation_options(~init_ctx, ~completions=settings.completions, ~db, z);
  print_gen_option(~db, gen_options);
  let mk_completions = mk_completions(~settings, ~db, z);
  let mk_left_convex = mk_new_left_convex(~settings, ~db, z);
  let mk_left_concave = mk_new_left_concave(~settings, ~db, z);
  switch (gen_options) {
  | NewRightConvex(info_dump) => {
      completions: [],
      new_tokens: mk_left_convex(info_dump),
    }
  | NewRightConcave(info_dump) => {
      completions: [],
      new_tokens: mk_left_concave(info_dump),
    }
  | CompletionOrNewRightConvex(info_dump_l, tok_to_left, info_dump_new) => {
      completions: mk_completions(~tok_to_left, info_dump_l),
      new_tokens: mk_left_convex(info_dump_new),
    }
  | CompletionOrNewRightConcave(info_dump, tok_to_left) => {
      completions: mk_completions(~tok_to_left, info_dump),
      new_tokens: mk_left_concave(info_dump),
    }
  | OnlyCompletion(info_dump, tok_to_left) => {
      completions: mk_completions(~tok_to_left, info_dump),
      new_tokens: [],
    }
  | OnlyCompletionString(_) => {
      completions: ["~EXTEND-STRINGLIT~"],
      new_tokens: [],
    }
  | OnlyCompletionComment(_) => {
      completions: ["~EXTEND-COMMENT~"],
      new_tokens: [],
    }
  };
};

let normalize_token = tok =>
  switch (tok) {
  | "~INTLIT~" => "intlit"
  | "~EXTEND-INTLIT~" => "extend-intlit"
  | "~FLOATLIT~" => "floatlit"
  | "~EXTEND-FLOATLIT~" => "extend-floatlit"
  | "~STRINGLIT~" => "stringlit"
  | "~EXTEND-STRINGLIT~" => "extend-stringlit"
  | "~EXTEND-COMMENT~" => "extend-comment"
  | "~PATVAR~" => "patvar"
  | "~EXTEND-PATVAR~" => "extend-patvar"
  | "~TYPVAR~" => "typvar"
  | "~EXTEND-TYPVAR~" => "extend-typvar"
  | "~CONSTRUCTOR~" => "constructor"
  | "~EXTEND-CONSTRUCTOR~" => "extend-constructor"
  | "~WHITESPACE~" => "whitespace"
  | "\\/" => {|"\\/"|}
  | tok => "\"" ++ tok ++ "\""
  };

let base_sorts = {|whitespace ::= [ \n]+
intlit ::= [0-9]+
extend-intlit ::= [0-9]+
floatlit ::= [0-9]+ "." [0-9]+
extend-floatlit ::= [0-9]* "." [0-9]+
stringlit ::= "\"" [^"]* "\""
extend-stringlit ::= [^"]* "\""
extend-comment ::= [^#]* "#"
patvar ::= [a-z][a-zA-Z0-9_]*
extend-patvar ::= [a-zA-Z0-9_]*
typvar ::= [A-Z][a-zA-Z0-9_]*
extend-typvar ::= [a-zA-Z0-9_]*
constructor ::= [A-Z][a-zA-Z0-9_]*
extend-constructor ::= [a-zA-Z0-9_]*
|};

let mk_grammar = (pre_grammar: pre_grammar): string => {
  let sort_completions = {
    let completions =
      pre_grammar.completions
      |> List.map(normalize_token)
      |> String.concat(" | ");
    pre_grammar.completions == [] ? "" : "\ncompletions ::= " ++ completions;
  };
  // Note addition of whitespace token
  let sort_new_tokens = {
    let new_tokens =
      ["~WHITESPACE~", ...pre_grammar.new_tokens]
      |> List.map(normalize_token)
      |> String.concat(" | ");
    pre_grammar.new_tokens == [] ? "" : "\nnew-tokens ::= " ++ new_tokens;
  };
  let sort_root =
    switch (pre_grammar.completions, pre_grammar.new_tokens) {
    | ([], []) => failwith("LSP: EXN: No completions or new tokens")
    | ([], _) => "new-tokens"
    | (_, []) => "completions"
    | _ => "completions | new-tokens"
    };
  base_sorts
  ++ sort_completions
  ++ sort_new_tokens
  ++ "\nroot ::= "
  ++ sort_root;
};

let go = (~db, ~settings: settings): unit => {
  let z =
    LSFiles.get_zipper(~db, settings.data.program, settings.data.new_token);

  print_endline("backpack-is-empty: " ++ string_of_bool(z.backpack == []));
  let grammar = z |> dispatch_generation(~settings, ~db) |> mk_grammar;
  db("LS: Grammar:");
  print_endline(grammar);
};

//MAYBE: consider reducing generation of duplicate lookahead suggestions
//MAYBE: abstract out default forms of any type eg case if let etc.
//MAYBE: Serialize prelude for speed
//MAYBE: make sure stopping completions doesn't prohibit completing unit (+ unit ap)

//TODO: Make sure synthetic mode is handled appropriately
//TODO: Type defintions are basically untested
//TODO: Type annotations and patterns are only lightly tested

//TODO(andrew): find cases where operator types go wrong
//BUG: "let _:String =\"yo\"+" only looks for ints so doesn't sug completion to "++"
//BUG: "\"yo\"+" only looks for ints so doesn't sug completion to "+."
//(above are monkeypatched to be too liberal for now)

//BUG: too general type in interior tuple elems (FIX: maybe actually insert commas at end)
// e.g. "let _:(Int, String, Float) =1," suggests everything

/* TODO: Restict new tokens and whitespace logic.
   1. (DONE)
   2. GATE backpack suggestion ON bidictx term ci has no errors
   3. GATE comma suggestion ON bidictx term ci has no _internal_ errors AND
      the bidictx analyzes against prefix of expected tuple */

/* TODO: More sophisticated logic for operand/operator insertion
      1. Operand Insertion
         0. Note that inserting an operand can't fix any errors, and
            neither can it bury ie render unfixable any existing errors (i think)
         a. If the operand analyzes against the type its parent expects for
            its right child, then we can insert it.
         b. Otherwise, we want to know if there exists an operator which could become its parent.
         c. That is, is there an infix or postfix operator, which binds tighter than the operator
            to the left of the operand (if any; if none, any infix or postfix operator will do),
            whose right child slot accept's the operand's type, and that itself will be consistent
            with the expectation of some possible chain of ancestors (infix and postfix operators)
            which can be inserted 'between' the operand and its parent.
         d. Operationally we approximate that last condition of c by asking getting the type chains
            for the current parent's right child, and seeing if any of them lead to the operand's type.
         e. If we extend the type chains with precdence information, then I think we can make this
            subsume (a) as well. We get the typechains (including the trivial one) for the operand's
           parent's right child type, see if any of them lead to the operand's type, and if so,
           then <INSERT SOME COMPLICATED PRECEDENCE LOGIC HERE>.
         f. Actually modulo the precedence logic, this is effectively the current logic i think.
     2. Operator Insertion:
        1. This can cause up to 2 errors, and fix up to 1 error. It can also cause errors to be
           buried, i.e. become unfixable.
        2. Assume for the moment that there are no errors to start.
        3. The for each prospective _infix_ operator, we can use its precedence to query the skel,
           and get its prospective parent and prospective child. the prospective left child
           must agree exactly as it cannot be re-parented. we then want their to be
           a typechain from prospective parent's right child slot to the new operator.
           <INSERT MAYBE MORE COMPLICATED PRECEDENCE LOGIC HERE>.
       4. (An Attempt): Something like... the prospectice parent is the nearest leftwards
          infix operator whose precdedence is looser than the new operator. for another operator
          to come between the two in the skel, it must have a looser precence that the new operator,
          but tighter than the prospective parent. so if we're considering inserting a sequence of
          operators to make the new operator agree with the parent, they all have to have prededences
          between the two (if they're infix, posffix are always fine i think?), and they have to be able
          to form a typechain. but i'm not sure if the typechain order has the same order so as to
          reduce this to predence monotonicty.
   */
