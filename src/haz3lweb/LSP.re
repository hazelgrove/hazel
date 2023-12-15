open Js_of_ocaml;
open Util;
open Haz3lcore;

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
//TODO: better handling of concaves
[@deriving (show({with_path: false}), sexp, yojson)]
type generation_options =
  | NewRightConvex(Id.t)
  | CompletionOrNewRightConvex(Id.t, string, Id.t)
  | NewRightConcave(Id.t) //id here is iffy
  | CompletionOrNewRightConcave(Id.t, string, Id.t); //snd id here is iffy

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

let hole_to_right = (~db, z: Zipper.t): option(Id.t) =>
  /* If we're doing pure left to right entry, there should be nothing to
     the right except for maybe a convex hole inserted by the grouter.
     If there is a such a hole, its CI should be used to inform new
     token insertions (but NOT completions of the token to the left) */
  switch (z.relatives.siblings |> snd) {
  | [] =>
    db("  LSP: Syntax: No rightwards piece");
    None;
  | [Grout({id, shape: Convex, _})] =>
    db("  LSP: Syntax: Rightwards piece is Convex Grout");
    Some(id);
  | [_, ..._] as rhs =>
    db("  LSP: Syntax: Rightwards segment is: " ++ Segment.show(rhs));
    failwith(
      "  LSP: Syntax: EXN: Nonempty Rightwards segment not single Convex Grout",
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
  switch (piece_to_left(~db, z), hole_to_right(~db, z)) {
  | (None, None) => failwith("LSP: EXN: Nothing to left or right")
  | (Some((to_left, shape, compl)), None) =>
    switch (shape) {
    | LeftConcave =>
      failwith("LSP: EXN: Concave to left and nothing to right")
    | LeftConvex =>
      switch (to_left, compl) {
      | (Just, Inert(id))
      | (SpacePadded, Completeable(id, _) | Inert(id)) =>
        NewRightConcave(id)
      | (Just, Completeable(id, left_token)) =>
        CompletionOrNewRightConcave(id, left_token, id)
      }
    }
  | (None, Some(id)) => NewRightConvex(id)
  | (Some((to_left, shape, compl)), Some(id_r)) =>
    switch (shape) {
    | LeftConvex => failwith("LSP: EXN: Convex to left and right")
    | LeftConcave =>
      switch (to_left, compl) {
      | (Just, Inert(_))
      | (SpacePadded, _) => NewRightConvex(id_r)
      | (Just, Completeable(id, left_tok)) =>
        CompletionOrNewRightConvex(id, left_tok, id_r)
      }
    }
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

let show_info = (db, ci: Info.t, _bidi_ci, z: Zipper.t) => {
  let sort = Info.sort_of(ci);
  let cls = Info.cls_of(ci);
  let ctx = Info.ctx_of(ci);
  let expected_ty = AssistantForms.Typ.expected(ci);
  let backpack_tokens = AssistantBackpack.to_token_list(z.backpack);
  db("  LSP: Info: cls: " ++ Term.Cls.show(cls));
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
  |> Aba.first_a  //TODO(andrew): is this right?
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

let unk = Typ.Unknown(Internal);

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

let concave_sugs = (ci: Info.t): Suggestion.s => {
  let infix_mono_sugs = AssistantForms.suggest_infix_mono(ci);
  let postfix_poly_sugs = AssistantForms.suggest_postfix_leading(ci);
  infix_mono_sugs @ postfix_poly_sugs;
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
  let concave_sugs = concave_sugs(ci);

  db(
    "LSP: Gen: Generating "
    ++ (
      completion != None
        ? "Completion" : "New Token (" ++ Nib.Shape.show(shape) ++ ")"
    )
    ++ " Suggestions",
  );

  show_info(db, ci, bidi_ci, z);

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

  //CHECK(andrew): make sure turning off types/ctx doesnt fuck up comma sugs
  //CHECK(andrew): check tydi handling of synthetic mode, ana unknown mode
  //CHECK(andrew): test type definitions esp ADTs
  //CHECK(andrew): consider reducing generation of duplicate lookahead suggestions
  //CHECK(andrew): find actually wrong type operator cases

  /* Too picky: */
  //BUG: Completions of regexp literals just returns same regexp

  /* Too liberal: */
  //BUG: postfix fn app suggested too liberally (in all exp contexts)
  //BUG: too general type in interior tuple elems (FIX: maybe actually insert commas at end)
  //BUG: "|" suggested too liberally (in all exp contexts)
  //FIX: only suggest if bidictx parent is case?

  /* TODO: Restict new tokens and whitespace logic:
      1. if prev token is a free variable/constructor which is not equal to a keyword,
      then prohibit new tokens + whitespace
      (if all according to keikaku, should always be a completion in this case;
      could for now liberalize and only prohibit if there are also completions)
      2. only make backpack suggestion if bidictx contains no errors
      3. only make comma suggestion if bidictx analyzes against prefix of expected tuple
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
    suggestions_concave
    @ suggestions_convex
    @ backpack_sugs
    |> List.map(Suggestion.content_of)
    |> List.filter_map(x =>
         if (String.starts_with(~prefix=tok_to_left, x)) {
           TyDi.suffix_of(x, tok_to_left);
         } else {
           /* TODO:
              1.This logic isn't quite right. Need to test whether tok_to_left
               is a valid prefix of thing, not just thing. Current broken cases
               include strings (basically all) and maybe some floats?
               2. Also partial intlits could also be floats; handled this but maybe
               more cases like this.
               3. I guess in general this should use regexps that check if tok_to_left
               is a prefix of a thing, and it so, return a suffix regexp.
               4. So basically need to write a prefix and suffix regexp for each
               5. technically above doesn't quite work since in general
               some prefixes might only have some valid suffixes
              */
           switch (x) {
           | "~INTLIT~" when Form.is_int(tok_to_left) => Some(x)
           | "~FLOATLIT~"
               when Form.is_float(tok_to_left) || Form.is_int(tok_to_left) =>
             Some(x)
           | "~STRINGLIT~" when Form.is_string(tok_to_left) => Some(x)
           | "~PATVAR~" when Form.is_var(tok_to_left) => Some(x)
           | "~TYPVAR~" when Form.is_typ_var(tok_to_left) => Some(x)
           | "~CONSTRUCTOR~" when Form.is_ctr(tok_to_left) => Some(x)
           | _ => None
           };
         }
       )
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
  if (z.caret != Outer) {
    failwith(
      "LSP: EXN: Caret position is inner: Segment before: "
      ++ Segment.show(seg_before)
      ++ "Segment after: "
      ++ Segment.show(seg_after),
    );
  };
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
  | CompletionOrNewRightConcave(id_l, string, id_new) =>
    db("  LSP: Syntax: Can insert new right-concave or complete left");
    db("  LSP: Syntax: Can insert new right-concave");
    let s1 =
      generate(~shape=Concave(0), ~completion=None, z, id_new)
      |> List.sort_uniq(String.compare);
    let s2 =
      generate(~shape=Convex, ~completion=Some(string), z, id_l)
      |> List.sort_uniq(String.compare);
    db("LSP: (1/2) New Token Suggestions: " ++ String.concat(" ", s1));
    db("LSP: (2/2) Completion Suggestions: " ++ String.concat(" ", s2));
    s1 @ s2;
  };
};

//TODO: get kevin to check these
//TODO: make whitespace conditional on something
let grammar_prefix = {|
intlit ::= [0-9]+
floatlit ::= [0-9]+ "." [0-9]+
stringlit ::= "\"" [^"]* "\""
patvar ::= [a-zA-Z_][a-zA-Z0-9_]*
typvar ::= [A-Z][a-zA-Z0-9_]*
constructor ::= [A-Z][a-zA-Z0-9_]*
whitespace ::= [ \n]+

root ::= whitespace | |};

let mk_grammar = (toks: list(string)): string =>
  List.map(
    tok =>
      switch (tok) {
      | "~INTLIT~" => "intlit"
      | "~FLOATLIT~" => "floatlit"
      | "~STRINGLIT~" => "stringlit"
      | "~PATVAR~" => "patvar"
      | "~TYPVAR~" => "typvar"
      | "~CONSTRUCTOR~" => "constructor"
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
