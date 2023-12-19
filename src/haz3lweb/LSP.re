open Js_of_ocaml;
open Util;
open Haz3lcore;

let unk = Typ.Unknown(Internal);

[@deriving (show({with_path: false}), sexp, yojson)]
type constrain =
  | Grammar
  | Context
  | Types;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  debug: bool,
  constrain,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type completability =
  | Completeable(Id.t, string)
  | Inert(Id.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type shapyness =
  | LeftConvex
  | LeftConcave;

[@deriving (show({with_path: false}), sexp, yojson)]
type thing_to_left =
  | Just
  | SpacePadded;

[@deriving (show({with_path: false}), sexp, yojson)]
type neighbor_info = (thing_to_left, shapyness, completability);

[@deriving (show({with_path: false}), sexp, yojson)]
type thing_to_right =
  | Nothing
  | ConvexHole(Id.t)
  | StringLit(string);

//TODO: better handling of concaves
[@deriving (show({with_path: false}), sexp, yojson)]
type generation_options =
  | OnlyCompletion(string)
  | NewRightConvex(Id.t)
  | CompletionOrNewRightConvex(Id.t, string, Id.t)
  | NewRightConcave(Id.t) //id here is iffy
  | CompletionOrNewRightConcave(Id.t, string); //snd id here is iffy

let getDefaultSettings = () => {
  {debug: false, constrain: Types};
};

let show_settings = (s: settings): string =>
  Printf.sprintf(
    "Debug: %b, Constrain: %s",
    s.debug,
    switch (s.constrain) {
    | Grammar => "Grammar"
    | Context => "Context"
    | Types => "Types"
    },
  );

let args = {
  ignore(Js.Unsafe.js_expr("require('process')"));
  //let num_args =
  //  Js.Unsafe.js_expr("process.argv.length") |> Js.parseInt |> string_of_int;
  //print_endline("num_args: " ++ num_args);
  switch (Js.Unsafe.js_expr("process.argv") |> Js.to_array) {
  | [||] => None
  | argv => Some(Array.to_list(Array.map(Js.to_string, argv)))
  };
};

let rec processArgs = (args, currentSettings) =>
  switch (args) {
  | [] => currentSettings
  | ["--debug", "false", ...rest] =>
    processArgs(rest, {...currentSettings, debug: false})
  | ["--debug", "true", ...rest] =>
    processArgs(rest, {...currentSettings, debug: true})
  | ["--constrain", "grammar", ...rest] =>
    processArgs(rest, {...currentSettings, constrain: Grammar})
  | ["--constrain", "context", ...rest] =>
    processArgs(rest, {...currentSettings, constrain: Context})
  | ["--constrain", "types", ...rest] =>
    processArgs(rest, {...currentSettings, constrain: Types})
  | [_arg, ...rest] => processArgs(rest, currentSettings) // Ignore unrecognized args
  };

let getLastArg = args =>
  switch (args) {
  | None
  | Some([]) => "" // Return empty string if no arguments
  | Some(listArgs) => List.hd(List.rev(listArgs)) // Extract the last element
  };

let getSettings = args =>
  switch (args) {
  | Some(realArgs) => processArgs(List.tl(realArgs), getDefaultSettings())
  | None => getDefaultSettings()
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

let thing_to_right = (~db, z: Zipper.t): thing_to_right =>
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
  | [_, ..._] as rhs =>
    db("  LSP: Syntax: Rightwards segment is: " ++ Segment.show(rhs));
    failwith(
      "  LSP: Syntax: EXN: Nonempty Rightwards segment not single Convex Grout or String literal",
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

let piece_to_left = (~db, z: Zipper.t): option(neighbor_info) =>
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

let generation_options = (~db, z: Zipper.t) => {
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
        NewRightConcave(id_l)
      | (Just, Completeable(id_l, left_token)) =>
        CompletionOrNewRightConcave(id_l, left_token)
      }
    }
  | (None, ConvexHole(id)) => NewRightConvex(id)
  | (Some((to_left, shape, compl)), ConvexHole(id_r)) =>
    switch (shape) {
    | LeftConvex => failwith("LSP: EXN: Convex to left and right")
    | LeftConcave =>
      switch (to_left, compl) {
      | (Just, Inert(_))
      | (SpacePadded, _) => NewRightConvex(id_r)
      | (Just, Completeable(id_l, left_tok)) =>
        CompletionOrNewRightConvex(id_l, left_tok, id_r)
      }
    }
  | (_, StringLit(id)) => OnlyCompletion(id)
  };
};

let lsp_z_to_ci =
    (~settings: CoreSettings.t, ~ctx: Ctx.t, id: Id.t, z: Zipper.t) => {
  let map =
    z
    |> MakeTerm.from_zip_for_sem
    |> fst
    |> Interface.Statics.mk_map_ctx(settings, ctx);
  Id.Map.find_opt(id, map);
};

let of_sugs: list(Suggestion.t) => string =
  s => s |> List.map((s: Suggestion.t) => s.content) |> String.concat(" ");

let error_str = ci =>
  switch (Info.error_of(ci)) {
  | Some(err) => Info.show_error(err)
  | None => "None"
  };

let show_info = (db, ci: Info.t, bidi_ci, bidi_parent_ci, z: Zipper.t) => {
  let sort = Info.sort_of(ci);
  let cls = Info.cls_of(ci);
  let ctx = Info.ctx_of(ci);
  let expected_ty = AssistantForms.Typ.expected(ci);
  let backpack_tokens = AssistantBackpack.to_token_list(z.backpack);
  db("  LSP: Info: cls: " ++ Term.Cls.show(cls));
  switch (bidi_ci) {
  | Some(ci) =>
    db("  LSP: Info: bidi_cls: " ++ Term.Cls.show(Info.cls_of(ci)))
  | None => db("  LSP: Info: No Bidi")
  };
  switch (bidi_parent_ci) {
  | Some(ci) =>
    db("  LSP: Info: bidi_parent_cls: " ++ Term.Cls.show(Info.cls_of(ci)))
  | None => db("  LSP: Info: bidi_parent: No Bidi Parent")
  };
  db("  LSP: Info: sort: " ++ Sort.to_string(sort));
  db("  LSP: Info: ctx: " ++ Ctx.to_string(ctx));
  db("  LSP: Info: Expected type: " ++ Typ.to_string(expected_ty));
  db("  LSP: Info: Error: " ++ error_str(ci));
  db("  LSP: Info: Backpack stack: " ++ String.concat(" ", backpack_tokens));
};

let get_bidi_id = (z: Zipper.t, indicated_id: Id.t) => {
  //let indicated_id = Indicated.index(z) |> Option.get;
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

let suggest_comma = (bidi_ctx_ci: Info.t) => {
  /*print_endline("suggest comma starting");
    print_endline("suggest comma. bidi_ctx_ci is " ++ Info.show(bidi_ctx_ci));
    switch (bidi_ctx_ci) {
    | InfoExp({term, _}) =>
      print_endline("suggest comma. term is " ++ Term.UExp.show(term))
    | InfoPat({term, _}) =>
      print_endline("suggest comma. term is " ++ Term.UPat.show(term))
    | InfoTyp({term, _}) =>
      print_endline("suggest comma. term is " ++ Term.UTyp.show(term))
    | _ => ()
    };*/
  switch (bidi_ctx_ci) {
  | InfoExp({mode: Syn, _})
  | InfoPat({mode: Syn, _}) => true
  | InfoExp({
      ctx,
      status: InHole(Common(Inconsistent(Expectation({ana, syn})))),
      _,
    })
  | InfoPat({
      ctx,
      status: InHole(Common(Inconsistent(Expectation({ana, syn})))),
      _,
    }) =>
    switch (ana, syn) {
    | (Prod(p_ana), Prod(p_syn)) =>
      // true if syn type p_syn is a prefix of the list expected_type p_ana
      // or more specifically, each type in p_syn should be consistent with it's
      // corresponding type in p_ana, up to the length of p_syn
      print_endline(
        "suggest comma case. p_ana is "
        ++ String.concat(" ", List.map(Typ.show, p_ana)),
      );
      print_endline(
        "suggest comma case. p_syn is "
        ++ String.concat(" ", List.map(Typ.show, p_syn)),
      );
      let rec is_strict_prefix = (p_syn, p_ana) =>
        switch (p_syn, p_ana) {
        | (_, []) => false
        | ([], _) => true
        | ([ty_syn, ...p_syn], [ty_ana, ...p_ana]) =>
          Typ.is_consistent(ctx, ty_syn, ty_ana)
          && is_strict_prefix(p_syn, p_ana)
        };
      is_strict_prefix(p_syn, p_ana);
    | (Prod([t1_ana, ..._]), t_syn) => Typ.is_consistent(ctx, t1_ana, t_syn)
    | _ => false
    }
  | InfoTyp(_) => true
  | _ => false
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

let convex_sugs = (~settings, ci: Info.t) =>
  switch (settings.constrain) {
  | Types =>
    switch (ci) {
    | InfoExp({mode, ctx, _}) =>
      suggest_exp(~fns=false, ctx, Mode.ty_of(mode))
    | InfoPat({mode, ctx, co_ctx, _}) =>
      suggest_pat(~fns=false, ctx, co_ctx, Mode.ty_of(mode))
    | InfoTyp({ctx, _}) => suggest_typ(ctx)
    | _ => []
    }
  | Context =>
    switch (ci) {
    | InfoExp({ctx, _}) => suggest_exp(~fns=false, ctx, unk)
    | InfoPat({ctx, co_ctx, _}) => suggest_pat(~fns=false, ctx, co_ctx, unk)
    | InfoTyp({ctx, _}) => suggest_typ(ctx)
    | _ => []
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
    | InfoTyp(_) => [Suggestion.mk("~TYPVAR~")]
    | _ => []
    }
  };

let convex_lookahead_sugs = (~settings, ~db, ci: Info.t) => {
  switch (settings.constrain) {
  | Context
  | Grammar => []
  | Types =>
    switch (ci) {
    | InfoExp({mode, ctx, _}) =>
      let ty = Mode.ty_of(mode);
      let ty_paths = AssistantCtx.get_lookahead_tys_exp(ty);
      db(
        "  LSP: Convex: Ty paths:\n " ++ AssistantCtx.show_type_path(ty_paths),
      );
      let tys =
        List.map(Util.ListUtil.last, ty_paths) |> List.sort_uniq(compare);
      // Filter out the current type
      //let tys = List.filter((!=)(ty), tys);
      db(
        "  LSP: Convex: Target types: "
        ++ (List.map(Typ.to_string, tys) |> String.concat(", ")),
      );
      suggest_exp(~fns=true, ctx, ty)
      @ (List.map(suggest_exp(~fns=true, ctx), tys) |> List.flatten);
    | InfoPat({mode, ctx, co_ctx, _}) =>
      let ty = Mode.ty_of(mode);
      let tys = AssistantCtx.get_lookahead_tys_pat(ty);
      suggest_pat(~fns=true, ctx, co_ctx, ty)
      @ (List.map(suggest_pat(~fns=true, ctx, co_ctx), tys) |> List.flatten);
    | InfoTyp(_) => []
    | _ => []
    }
  };
};

let postfix_sugs =
    (
      ~settings,
      ~db as _,
      ci: Info.t,
      bidi_ci: option(Info.t),
      bidi_parent_ci: option(Info.t),
    )
    : Suggestion.s => {
  let case_rule_sug = {
    let cls1 = bidi_parent_ci |> Option.map(Info.cls_of);
    let cls2 = bidi_ci |> Option.map(Info.cls_of);
    /* NOTE: We have to check both the bidictx and the parent here
       because the way case is implemented, rule tiles get the ci for
       the whole case, so if we're on a rule tile, the bidictx will be
       the whole case. (TODO: This is confusing, clarify) */
    cls1 == Some(Exp(Match)) || cls2 == Some(Exp(Match))
      ? [Suggestion.mk("|")] : [];
  };
  let postfix_ap_sug = (ctx, self_ty) =>
    /* Could alternatively make this more restrictive and require
       that actually arrow type not merely consistent. This would
       enforce use with appropriate constructor in patterns,
       but would screw up current impl of grammar/context-only generation */
    switch (
      self_ty
      |> Option.map(self_ty =>
           Typ.is_consistent(ctx, self_ty, Arrow(unk, unk))
         )
    ) {
    | Some(true) => [Suggestion.mk("(")]
    | _ => []
    };
  switch (settings.constrain) {
  | Grammar =>
    switch (ci) {
    | InfoExp(_) => postfix_ap_sug([], Some(unk)) @ case_rule_sug
    | InfoPat(_) => postfix_ap_sug([], Some(unk))
    | InfoTyp(_) =>
      //TODO: make more ap more restrictive?
      [Suggestion.mk("(")]
    | _ => []
    }
  | Context =>
    let ctx = Info.ctx_of(ci);
    switch (ci) {
    | InfoExp(_) => postfix_ap_sug(ctx, Some(unk)) @ case_rule_sug
    | InfoPat(_) => postfix_ap_sug(ctx, Some(unk))
    | InfoTyp(_) =>
      //TODO: make more ap more restrictive?
      [Suggestion.mk("(")]
    | _ => []
    };
  | Types =>
    switch (ci) {
    | InfoExp({ctx, self, _}) =>
      let self_ty = Self.typ_of_exp(Info.ctx_of(ci), self);
      postfix_ap_sug(ctx, self_ty) @ case_rule_sug;
    | InfoPat({ctx, self, _}) =>
      let self_ty = Self.typ_of_pat(Info.ctx_of(ci), self);
      postfix_ap_sug(ctx, self_ty);
    | InfoTyp(_) =>
      //TODO: make more ap more restrictive?
      [Suggestion.mk("(")]
    | _ => []
    }
  };
};

let sug_exp_infix = (ctx: Ctx.t, l_child_ty: Typ.t, expected_ty: Typ.t) => {
  let b =
    if (Typ.is_consistent(ctx, expected_ty, Bool)) {
      let b1 =
        Typ.is_consistent(ctx, l_child_ty, Bool)
          ? [Suggestion.mk("&&"), Suggestion.mk("\\/")] : [];
      let b2 =
        Typ.is_consistent(ctx, l_child_ty, String)
          ? [Suggestion.mk("$==")] : [];
      let b3 =
        Typ.is_consistent(ctx, l_child_ty, Int)
          ? [
            Suggestion.mk("=="),
            Suggestion.mk("!="),
            Suggestion.mk("<="),
            Suggestion.mk(">="),
            Suggestion.mk("<"),
            Suggestion.mk(">"),
          ]
          : [];
      let b4 =
        Typ.is_consistent(ctx, l_child_ty, Float)
          ? [
            Suggestion.mk("==."),
            Suggestion.mk("!=."),
            Suggestion.mk("<=."),
            Suggestion.mk(">=."),
            Suggestion.mk("<."),
            Suggestion.mk(">."),
          ]
          : [];
      b1 @ b2 @ b3 @ b4;
    } else {
      [];
    };
  let i =
    if (Typ.is_consistent(ctx, expected_ty, Int)
        && Typ.is_consistent(ctx, l_child_ty, Int)) {
      [
        Suggestion.mk("+"),
        Suggestion.mk("-"),
        Suggestion.mk("*"),
        Suggestion.mk("/"),
        Suggestion.mk("**"),
      ];
    } else {
      [];
    };
  let f =
    if (Typ.is_consistent(ctx, expected_ty, Float)
        && Typ.is_consistent(ctx, l_child_ty, Float)) {
      [
        Suggestion.mk("+."),
        Suggestion.mk("-."),
        Suggestion.mk("*."),
        Suggestion.mk("/."),
        Suggestion.mk("**."),
      ];
    } else {
      [];
    };
  let s =
    if (Typ.is_consistent(ctx, expected_ty, String)
        && Typ.is_consistent(ctx, l_child_ty, String)) {
      [Suggestion.mk("++")];
    } else {
      [];
    };
  let l =
    if (Typ.is_consistent(ctx, expected_ty, List(unk))) {
      let l1 =
        Typ.is_consistent(
          ctx,
          l_child_ty,
          Typ.matched_list(ctx, expected_ty),
        )
          ? [Suggestion.mk("::")] : [];
      /*Note: Using List below because don't want this
        check to pass if expected is unknown but
        l_child is e.g. Int */
      let l2 =
        Typ.is_consistent(
          ctx,
          l_child_ty,
          List(Typ.matched_list(ctx, expected_ty)),
        )
          ? [Suggestion.mk("@")] : [];
      l1 @ l2;
    } else {
      [];
    };
  b @ i @ f @ s @ l;
};

let infix_sugs =
    (~settings, ~db, ci: Info.t, bidi_ci: option(Info.t)): Suggestion.s => {
  let infix_mono_sugs = AssistantForms.suggest_infix_mono(ci);
  switch (settings.constrain) {
  | Grammar =>
    switch (ci) {
    | InfoExp(_) => sug_exp_infix([], unk, unk)
    | InfoPat(_)
    | InfoTyp(_) => infix_mono_sugs
    | _ => []
    }
  | Context =>
    switch (ci) {
    | InfoExp({ctx, _}) => sug_exp_infix(ctx, unk, unk)
    | InfoPat(_)
    | InfoTyp(_) => infix_mono_sugs
    | _ => []
    }
  | Types =>
    switch (ci) {
    | InfoExp({ctx, mode, self, _}) =>
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
      let expected_ty = Mode.ty_of(mode);
      let self_ty =
        switch (Self.typ_of_exp(ctx, self)) {
        | Some(ty) => ty
        | None => unk
        };
      db(
        "  LSP: Concave: Infix: Left child Self type: "
        ++ Typ.to_string(self_ty),
      );
      let base = sug_exp_infix(ctx, self_ty, expected_ty);
      db("  LSP: Concave: Infix: Base: " ++ of_sugs(base));
      let bidi_ctx_expected_ty =
        switch (bidi_ci) {
        | Some(InfoExp({mode, _}))
        | Some(InfoPat({mode, _})) => Mode.ty_of(mode)
        | _ => expected_ty
        };
      let ty_paths = AssistantCtx.get_lookahead_tys_exp(bidi_ctx_expected_ty);
      let tys =
        List.map(Util.ListUtil.last, ty_paths) |> List.sort_uniq(compare);
      db(
        "  LSP: Concave: Infix: Lookahead types: "
        ++ (List.map(Typ.to_string, tys) |> String.concat(", ")),
      );
      let lookahead =
        List.map(sug_exp_infix(ctx, self_ty), tys) |> List.flatten;
      db(
        "  LSP: Concave: Infix: Lookahead: " ++ of_sugs(lookahead |> dedup),
      );
      base @ lookahead;
    //TODO: get self_ty of actual prospective child, on a per-operator basis
    | InfoPat(_)
    | InfoTyp(_) => infix_mono_sugs
    | _ => []
    }
  };
};

let concave_sugs =
    (
      ~settings,
      ~db,
      ci: Info.t,
      bidi_ci: option(Info.t),
      bidi_parent_ci: option(Info.t),
    )
    : Suggestion.s => {
  let infix_sugs = infix_sugs(~settings, ~db, ci, bidi_ci);
  let postfix_sugs =
    postfix_sugs(~settings, ~db, ci, bidi_ci, bidi_parent_ci);
  db("  LSP: Concave: Postfix: " ++ of_sugs(postfix_sugs));
  infix_sugs @ postfix_sugs;
};

let completion_filter =
    (caret: Zipper.Caret.t, tok_to_left: string, sug: string) => {
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

let generate =
    (
      ~settings: settings,
      ~db,
      ~shape: Nib.Shape.t,
      ~completion: option(string),
      z: Zipper.t,
      id: Id.t,
    )
    : list(string) => {
  let get_info = id =>
    lsp_z_to_ci(
      ~settings=CoreSettings.on,
      ~ctx=[], //Builtins.ctx_init,
      id,
      z,
    );
  let ci =
    OptUtil.get_or_fail(
      "LSP: Gen: EXN: Couldn't find CI for id " ++ Id.to_string(id),
      get_info(id),
    );
  //TODO: de-option below
  let bidi_ci = get_info(get_bidi_id(z, id));
  let bidi_parent_ci =
    bidi_ci
    |> Option.map(Info.ancestors_of)
    |> OptUtil.and_then(ListUtil.hd_opt)
    |> OptUtil.and_then(get_info);

  let n_ary_sugs: list(Suggestion.t) = {
    let comma_sug = Suggestion.mk(",");
    switch (settings.constrain) {
    | Types =>
      switch (Option.map(suggest_comma, bidi_ci)) {
      | Some(true) =>
        db("  LSP: Syntax: Suggesting comma");
        [comma_sug];
      | Some(false) =>
        db("  LSP: Syntax: Not Suggesting comma");
        [];
      | _ =>
        db("  LSP: Syntax: Error: Bidictx id not found");
        [];
      }
    | Context
    | Grammar => [comma_sug]
    };
  };
  let backpack_sugs = AssistantBackpack.suggest(z);
  let convex_backpack_sugs =
    List.filter_map(AssistantBackpack.is_convex, backpack_sugs);
  let concave_backpack_sugs =
    List.filter_map(AssistantBackpack.is_concave, backpack_sugs);
  let convex_sugs = convex_sugs(~settings, ci);
  let convex_lookahead_sugs = convex_lookahead_sugs(~settings, ~db, ci);
  let concave_sugs =
    concave_sugs(~settings, ~db, ci, bidi_ci, bidi_parent_ci);

  db(
    "LSP: Gen: Generating "
    ++ (
      completion != None
        ? "Completion" : "New Token (" ++ Nib.Shape.show(shape) ++ ")"
    )
    ++ " Suggestions",
  );

  show_info(db, ci, bidi_ci, bidi_parent_ci, z);

  switch (shape) {
  | Convex =>
    db("  LSP: Base: Convex: Backpack: " ++ of_sugs(convex_backpack_sugs));
    db("  LSP: Base: Convex: " ++ of_sugs(convex_sugs));
    db(
      "  LSP: Base: Convex: Lookahead: "
      ++ of_sugs(convex_lookahead_sugs |> dedup),
    );
  | Concave(_) =>
    db("  LSP: Base: Concave: Backpack: " ++ of_sugs(concave_backpack_sugs));
    db("  LSP: Base: Concave: N-ary: " ++ of_sugs(n_ary_sugs));
    db("  LSP: Base: Concave: " ++ of_sugs(concave_sugs));
  };

  //MAYBE: consider reducing generation of duplicate lookahead suggestions
  //MAYBE: abstract out default forms of any type eg case if let etc.

  //TODO: Make sure synthetic mode is handled appropriately
  //TODO: Type defintions are basically untested
  //TODO: Type annotations and patterns are only lightly tested

  //TODO(andrew): find cases where operator types go wrong

  //BUG: too general type in interior tuple elems (FIX: maybe actually insert commas at end)

  /* TODO: Restict new tokens and whitespace logic.
     Let 'NEW' below refer to both new tokens and whitespace.
     We gate NEW tokens on the following:
      1. if tile-to-left has a Free or Invalid Error AND not equal to keyword
         a. if in Types or Context mode, prohibit NEW tokens + whitespace.
         b. if in Grammar mode, do this only fo Invalid Tokens
         c. We should also ASSERT that there are actually completions in such cases
         d. think harder about the keyword case.
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

  let suggestions_convex =
    convex_backpack_sugs
    @ convex_sugs
    @ convex_lookahead_sugs
    |> List.sort_uniq(Suggestion.compare);

  let suggestions_concave =
    concave_backpack_sugs
    @ n_ary_sugs
    @ concave_sugs
    |> List.sort_uniq(Suggestion.compare);

  switch (completion) {
  | None =>
    let base_suggestions =
      switch (shape) {
      | Convex => suggestions_convex
      | Concave(_) => suggestions_concave
      };
    List.map(Suggestion.content_of, base_suggestions);
  | Some(tok_to_left) =>
    /* Note that for completion suggestions, we don't want to case
       on shape expectation, because the tile might get remolded */
    db("  LSP: Completion: tok_to_left: " ++ tok_to_left);
    suggestions_concave
    @ suggestions_convex
    @ backpack_sugs
    |> List.map(Suggestion.content_of)
    |> List.filter_map(completion_filter(z.caret, tok_to_left));
  };
};

let dispatch_generation = (~settings: settings, ~db, s: string): list(string) => {
  let generate = generate(~settings, ~db);
  db("LSP: Init: Recieved string: " ++ s);
  let z =
    OptUtil.get_or_fail(
      "LSP: Init: EXN: Couldn't parse string",
      Printer.zipper_of_string(s),
    );
  db("LSP: Init: String parsed successfully");
  let seg_before = z.relatives.siblings |> fst |> List.rev;
  let seg_after = z.relatives.siblings |> snd;
  if (seg_before == [] && seg_after == []) {
    failwith("LSP: EXN: Empty segment");
  };
  switch (generation_options(~db, z)) {
  | NewRightConvex(id) =>
    db("  LSP: Syntax: Can insert new right-convex");
    let sugs =
      generate(~shape=Convex, ~completion=None, z, id)
      |> List.sort_uniq(String.compare);
    db(
      "LSP: Final: (1/1) New right-convex Suggestions: "
      ++ String.concat(" ", sugs),
    );
    sugs;
  | NewRightConcave(id) =>
    let sugs =
      generate(~shape=Concave(0), ~completion=None, z, id)
      |> List.sort_uniq(String.compare);
    db(
      "LSP: Final: (1/1) New right-concave Suggestions: "
      ++ String.concat(" ", sugs),
    );
    sugs;
  | CompletionOrNewRightConvex(id_l, string, id_new) =>
    db("  LSP: Syntax: Can insert new right-convex or complete left");
    let s1 =
      generate(~shape=Convex, ~completion=None, z, id_new)
      |> List.sort_uniq(String.compare);
    let s2 =
      generate(~shape=Concave(0), ~completion=Some(string), z, id_l)
      |> List.sort_uniq(String.compare);
    db(
      "LSP: Final: (1/2) New Token Suggestions: " ++ String.concat(" ", s1),
    );
    db(
      "LSP: Final: (2/2) Completion Suggestions: " ++ String.concat(" ", s2),
    );
    s1 @ s2;
  | CompletionOrNewRightConcave(id_l, string) =>
    db("  LSP: Syntax: Can insert new right-concave or complete left");
    db("  LSP: Syntax: Can insert new right-concave");
    let s1 =
      generate(~shape=Concave(0), ~completion=None, z, id_l)
      |> List.sort_uniq(String.compare);
    let s2 =
      generate(~shape=Convex, ~completion=Some(string), z, id_l)
      |> List.sort_uniq(String.compare);
    db("LSP: (1/2) New Token Suggestions: " ++ String.concat(" ", s1));
    db("LSP: (2/2) Completion Suggestions: " ++ String.concat(" ", s2));
    s1 @ s2;
  | OnlyCompletion(_stringlit) => [
      Suggestion.mk("~EXTEND-STRINGLIT~") |> Suggestion.content_of,
    ]
  };
};

let grammar_prefix = {|
intlit ::= [0-9]+
extend-intlit ::= [0-9]+
floatlit ::= [0-9]+ "." [0-9]+
extend-floatlit ::= [0-9]* "." [0-9]+
stringlit ::= "\"" [^"]* "\""
extend-stringlit ::= [^"]* "\""
patvar ::= [a-z][a-zA-Z0-9_]*
extend-patvar ::= [a-zA-Z0-9_]*
typvar ::= [A-Z][a-zA-Z0-9_]*
extend-typvar ::= [a-zA-Z0-9_]*
constructor ::= [A-Z][a-zA-Z0-9_]*
extend-constructor ::= [a-zA-Z0-9_]*

whitespace ::= [ \n]+

root ::= whitespace | |};

let mk_grammar = (toks: list(string)): string =>
  List.map(
    tok =>
      switch (tok) {
      | "~INTLIT~" => "intlit"
      | "~EXTEND-INTLIT~" => "extend-intlit"
      | "~FLOATLIT~" => "floatlit"
      | "~EXTEND-FLOATLIT~" => "extend-floatlit"
      | "~STRINGLIT~" => "stringlit"
      | "~EXTEND-STRINGLIT~" => "extend-stringlit"
      | "~PATVAR~" => "patvar"
      | "~EXTEND-PATVAR~" => "extend-patvar"
      | "~TYPVAR~" => "typvar"
      | "~EXTEND-TYPVAR~" => "extend-typvar"
      | "~CONSTRUCTOR~" => "constructor"
      | "~EXTEND-CONSTRUCTOR~" => "extend-constructor"
      | "\\/" => {|"\\/"|}
      | tok => "\"" ++ tok ++ "\""
      },
    toks,
  )
  |> String.concat(" | ")
  |> (toks => grammar_prefix ++ toks);

let main = (settings: settings, s: string) => {
  let db = s => settings.debug ? print_endline(s) : ();
  db(show_settings(settings));
  let sugs = dispatch_generation(~settings, ~db, s);
  db("LSP: Grammar:\n ");
  print_endline(mk_grammar(sugs));
};

main(getSettings(args), getLastArg(args));
