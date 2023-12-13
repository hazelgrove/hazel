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

type tileness =
  | Monotile(Id.t, string)
  | Polytile(Id.t);

type shapyness =
  | LeftConvex(tileness)
  | LeftConcave(tileness);

type thing_to_left =
  | Nothing
  | Just(shapyness)
  | SpacesThen(shapyness);

//TODO: better handling of concaves
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

let piece_to_left = (~db, z: Zipper.t): thing_to_left =>
  /*
   Returning Left means we're looking for either a new thing
    that starts with a left-facing chevron, or a completion of the
    thing to the left, which has a left-side right-facing chevron.
    */
  switch (z.relatives.siblings |> fst |> List.rev) {
  | [] => Nothing
  | [lht, ..._] as seg =>
    if (Piece.is_secondary(lht)) {
      db("  LSP: Syntax Inspector: Leftward piece is Secondary; trimming");
    };
    switch (Segment.trim_secondary(Left, seg)) {
    | [] =>
      failwith(
        "  LSP: Syntax: EXN: Rightwards segment after trimming secondaries",
      )
    | [lht, ..._] =>
      switch (lht) {
      | Tile({label: [], _}) => failwith("LSP: EXN: Tile with empty label")
      | Tile({label: [tok_to_left], id, _} as t) =>
        db("  LSP: Syntax: Leftward piece is Monotile: " ++ tile_str(t));
        let g =
          switch (right_nib_dir(t)) {
          | Right => LeftConvex(Monotile(id, tok_to_left))
          | Left => LeftConcave(Monotile(id, tok_to_left))
          };
        Piece.is_secondary(lht) ? SpacesThen(g) : Just(g);
      | Tile(t) =>
        db(
          "  LSP: Syntax: Leftward piece is "
          ++ (Tile.is_complete(t) ? "Complete" : "Incomplete")
          ++ " Polytile: "
          ++ tile_str(t),
        );
        let g =
          switch (right_nib_dir(t)) {
          | Right => LeftConvex(Polytile(t.id))
          | Left => LeftConcave(Polytile(t.id))
          };
        Piece.is_secondary(lht) ? SpacesThen(g) : Just(g);
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
  | (Nothing, None) => failwith("LSP: EXN: Nothing to left or right")
  | (Just(LeftConcave(_)) | SpacesThen(LeftConcave(_)), None) =>
    failwith("LSP: EXN: Concave to left and nothing to right")
  | (Just(LeftConvex(_)) | SpacesThen(LeftConvex(_)), Some(_)) =>
    failwith("LSP: EXN: Convex to left and right")
  | (Nothing | Just(LeftConcave(Polytile(_))) | SpacesThen(_), Some(id)) =>
    NewRightConvex(id)
  | (Just(LeftConcave(Monotile(id, left_token))), Some(id_to_right)) =>
    CompletionOrNewRightConvex(id, left_token, id_to_right)
  | (
      Just(LeftConvex(Polytile(id))) |
      SpacesThen(LeftConvex(Polytile(id) | Monotile(id, _))),
      None,
    ) =>
    //TODO: id here is weird
    NewRightConcave(id)
  | (Just(LeftConvex(Monotile(id, left_token))), None) =>
    //TODO: id here is weird
    CompletionOrNewRightConcave(id, left_token, id)
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

let show_info = (db, ci: Info.t, z: Zipper.t) => {
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
      status:
        InHole(
          Common(
            Inconsistent(
              Expectation({ana: Prod(p_ana), syn: Prod(p_syn)}),
            ),
          ),
        ),
      _,
    })
  | InfoPat({
      ctx,
      status:
        InHole(
          Common(
            Inconsistent(
              Expectation({ana: Prod(p_ana), syn: Prod(p_syn)}),
            ),
          ),
        ),
      _,
    }) =>
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
  | InfoExp({
      ctx,
      status:
        InHole(
          Common(
            Inconsistent(
              Expectation({ana: Prod([t1_ana, ..._]), syn: t_syn}),
            ),
          ),
        ),
      _,
    })
  | InfoPat({
      ctx,
      status:
        InHole(
          Common(
            Inconsistent(
              Expectation({ana: Prod([t1_ana, ..._]), syn: t_syn}),
            ),
          ),
        ),
      _,
    }) =>
    Typ.is_consistent(ctx, t1_ana, t_syn)
  | InfoTyp(_) => true
  | _ => false
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
  let ctx_init = []; //Builtins.ctx_init;
  let ci =
    OptUtil.get_or_fail(
      "LSP: Gen: EXN: Couldn't find CI for id " ++ Id.to_string(id),
      lsp_z_to_ci(~settings=CoreSettings.on, ~ctx=ctx_init, id, z),
    );

  //TODO: only make backpack sug if bidictx is NotInHole
  // or maybe doesn't have any errors at all?
  let backpack_sug = AssistantBackpack.suggest(z);
  /*let convex_const_mono_sugs = AssistantForms.suggest_const_mono(ci);
    let convex_abstract_mono_sugs = AssistantForms.suggest_abstract_mono(ci);
    let prefix_mono_sugs = AssistantForms.suggest_prefix_mono(ci);
    let prefix_poly_sugs = AssistantForms.suggest_prefix_leading(ci);*/
  let n_ary_sugs: list(Suggestion.t) = {
    //print_endline("bidi ctx id:" ++ Id.to_string(get_bidi_id(z, id)));
    let bidi_ci =
      lsp_z_to_ci(
        ~settings=CoreSettings.on,
        ~ctx=Builtins.ctx_init,
        get_bidi_id(z, id),
        z,
      );
    let blah = bidi_ci |> Option.map(suggest_comma);
    switch (blah) {
    | Some(true) =>
      db("  LSP: Syntax: Suggesting comma");
      [Suggestion.{content: ",", strategy: Default}];
    | Some(false) =>
      db("  LSP: Syntax: Not Suggesting comma");
      [];
    | _ =>
      db("  LSP: Syntax: Error: Id not found");
      [];
    };
  };
  let suggest_exp = (ctx, ty) =>
    AssistantCtx.suggest_bound_exp(ty, ctx)
    @ AssistantForms.suggest_all_ty_convex(Exp, ctx, ty);
  let suggest_pat = (ctx, co_ctx, ty) =>
    AssistantCtx.suggest_free_var(ty, ctx, co_ctx)
    @ AssistantCtx.suggest_bound_pat(ty, ctx)
    @ AssistantForms.suggest_all_ty_convex(Pat, ctx, ty);
  let unk = Typ.Unknown(Internal);
  let convex_var_sugs =
    switch (settings.constrain) {
    | Types =>
      switch (ci) {
      | InfoExp({mode, ctx, _}) => suggest_exp(ctx, Mode.ty_of(mode))
      | InfoPat({mode, ctx, co_ctx, _}) =>
        suggest_pat(ctx, co_ctx, Mode.ty_of(mode))
      | InfoTyp({ctx, _}) => AssistantCtx.suggest_bound_typ(ctx)
      | _ => []
      }
    | Context =>
      switch (ci) {
      | InfoExp({ctx, _}) => suggest_exp(ctx, unk)
      | InfoPat({ctx, co_ctx, _}) => suggest_pat(ctx, co_ctx, unk)
      | InfoTyp({ctx, _}) => AssistantCtx.suggest_bound_typ(ctx)
      | _ => []
      }
    | Grammar =>
      switch (ci) {
      | InfoExp(_) =>
        [Suggestion.{content: "~PATVAR~", strategy: Default}]
        @ [Suggestion.{content: "~CONSTRUCTOR~", strategy: Default}]
        @ suggest_exp([], unk)
      | InfoPat(_) =>
        [Suggestion.{content: "~CONSTRUCTOR~", strategy: Default}]
        @ suggest_pat([], [], unk)
      | InfoTyp(_) => []
      | _ => []
      }
    };

  let nu_convex_lookahead_var_sugs =
    switch (settings.constrain) {
    | Context
    | Grammar => []
    | Types =>
      switch (ci) {
      | InfoExp({mode, ctx, _}) =>
        let ty_paths = AssistantCtx.get_lookahead_tys_exp(Mode.ty_of(mode));
        db(
          "  LSP: Convex: Ty paths:\n "
          ++ AssistantCtx.show_type_path(ty_paths),
        );
        let tys =
          List.map(Util.ListUtil.last, ty_paths) |> List.sort_uniq(compare);
        db(
          "  LSP: Convex: Target types: "
          ++ (List.map(Typ.to_string, tys) |> String.concat(", ")),
        );
        List.map(suggest_exp(ctx), tys) |> List.flatten;
      | InfoPat({mode, ctx, co_ctx, _}) =>
        let tys = AssistantCtx.get_lookahead_tys_pat(Mode.ty_of(mode));
        List.map(suggest_pat(ctx, co_ctx), tys) |> List.flatten;
      | InfoTyp({ctx, _}) => AssistantCtx.suggest_bound_typ(ctx)
      | _ => []
      }
    };
  let convex_lookahead_var_sugs =
    switch (settings.constrain) {
    | Context
    | Grammar => []
    | Types =>
      switch (ci) {
      | InfoExp({mode, ctx, _}) =>
        let ty = Mode.ty_of(mode);
        AssistantCtx.suggest_lookahead_variable_exp(ty, ctx)
        @ AssistantForms.suggest_all_ty_convex(Exp, ctx, ty);
      | InfoPat({mode, ctx, co_ctx, _}) =>
        let ty = Mode.ty_of(mode);
        AssistantCtx.suggest_lookahead_variable_pat(ty, ctx, co_ctx)
        @ AssistantForms.suggest_all_ty_convex(Pat, ctx, ty);
      | InfoTyp(_) => []
      | _ => []
      }
    };
  let infix_mono_sugs = AssistantForms.suggest_infix_mono(ci);
  let postfix_poly_sugs = AssistantForms.suggest_postfix_leading(ci);
  db(
    "LSP: Gen: Generating "
    ++ (
      completion != None
        ? "Completion" : "New Token (" ++ Nib.Shape.show(shape) ++ ")"
    )
    ++ " Suggestions",
  );
  show_info(db, ci, z);
  db("  LSP: Base: Backpack suggestion: " ++ of_sugs(backpack_sug));
  db("  LSP: Base: Mono Convex Vars: " ++ of_sugs(convex_var_sugs));
  db(
    "  LSP: Base: Mono Convex Lookahead Vars: "
    ++ of_sugs(convex_lookahead_var_sugs),
  );
  db(
    "  LSP: Base: NU Mono Convex Lookahead Vars: "
    ++ of_sugs(nu_convex_lookahead_var_sugs),
  );
  /*db("  LSP: Base: Mono Convex Const: " ++ of_sugs(convex_const_mono_sugs));
    db(
      "  LSP: Base: Mono Convex Abstract: "
      ++ of_sugs(convex_abstract_mono_sugs),
    );
    db("  LSP: Base: Mono Prefix: " ++ of_sugs(prefix_mono_sugs));
    db("  LSP: Base: Poly Prefix: " ++ of_sugs(prefix_poly_sugs));*/
  db("  LSP: Base: Mono Infix: " ++ of_sugs(infix_mono_sugs));
  db("  LSP: Base: Poly Postfix: " ++ of_sugs(postfix_poly_sugs));
  db("  LSP: Base: N-ary: " ++ of_sugs(n_ary_sugs));

  //TODO(andrew): only suggest "|" if immediate parent is case?
  //TODO(andrew): restriction postfix fn application somehow
  //TODO(andrew): get rid ? of wild underscore in exp
  //TODO(andrew): make sure turning off types/ctx doesnt fuck up comma sugs
  //TODO(andrew): test type definitions esp ADTs

  //TODO(andrew): check tydi handling of synthetic pos, unknown type
  //TODO(andrew): completing fns apps incl empty ap
  //TODO(andrew): case rules suggested everywhere in expr
  //TODO(andrew): not all leading necessarily convex... handle principled
  //TODO(andrew): completing ( and [ into () and []
  //TODO(andrew): things with unknown type get duplicated lookahead sugs?
  //TODO(andrew): lookahead case: technically "let a:Int = (f" should suggest fun
  //TODO(andrew): if unbound error and exist completions, maybe dont sug ws or other new tok
  //TODO(andrew): cases of being halfway through int/float/stringlits or pat/tyvars

  //TODO(andrew): if on type inconsistency error, suggest everything for now
  // (but not if on unbound error)

  // from kevin:
  //TODO(andrew): logic for completing free forms regexps
  // if expected type consistent with tuple, suggest ,
  // if expecte type consistent with tuple, suggest first type
  // (in effect we get no type guidance for remaining members of tuple)
  // cut off lookaheads at space

  let suggestions_convex =
    /*convex_const_mono_sugs
      @ convex_abstract_mono_sugs
      @ prefix_mono_sugs
      @ prefix_poly_sugs
      @ */ convex_var_sugs
    @ convex_lookahead_var_sugs
    @ nu_convex_lookahead_var_sugs
    |> List.sort(Suggestion.compare);

  let suggestions_concave =
    n_ary_sugs
    @ infix_mono_sugs
    @ postfix_poly_sugs
    |> List.sort(Suggestion.compare);

  switch (completion) {
  | None =>
    let base_suggestions =
      switch (shape) {
      | Convex =>
        suggestions_convex
        @ List.filter_map(AssistantBackpack.is_convex, backpack_sug)
      | Concave(_) =>
        suggestions_concave
        @ List.filter_map(AssistantBackpack.is_concave, backpack_sug)
      };
    List.map(Suggestion.content_of, base_suggestions);
  | Some(tok_to_left) =>
    /* Note that for completion suggestions, we don't want to case
       on shape expectation, because the tile might get remolded */
    suggestions_concave
    @ suggestions_convex
    @ backpack_sug
    |> List.map(Suggestion.content_of)
    |> List.filter(String.starts_with(~prefix=tok_to_left))
    |> List.filter_map(x => TyDi.suffix_of(x, tok_to_left))
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
