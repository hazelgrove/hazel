open Js_of_ocaml;
open Util.OptUtil.Syntax;
open Haz3lcore;
open Haz3lcore.TyDi;

Js.Unsafe.js_expr("require('process')");
let arg_str = Js.Unsafe.js_expr("process.argv[2]") |> Js.to_string;
print_endline("LSP: recieved string: " ++ arg_str);

/* assume for now left-to-right entry, so the present shards are
   a prefix of the complete tile. this means that regardless of
   completeness, we can just use the left nibs */

let left_nib_dir = (t: Tile.t): Util.Direction.t =>
  fst(t.mold.nibs).shape == Convex ? Left : Right;
let right_nib_dir = (t: Tile.t): Util.Direction.t =>
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

let hole_to_right = (z: Zipper.t): option(Id.t) =>
  /* If we're doing pure left to right entry, there should be nothing to
     the right except for maybe a convex hole inserted by the grouter.
     If there is a such a hole, its CI should be used to inform new
     token insertions (but NOT completions of the token to the left) */
  switch (z.relatives.siblings |> snd) {
  | [] =>
    print_endline("LSP: No rightwards piece");
    None;
  | [Grout({id, shape: Convex, _})] =>
    print_endline("LSP: Rightwards piece is Convex Grout");
    Some(id);
  | [_, ..._] as rhs =>
    print_endline("LSP: Rightwards segment is: " ++ Segment.show(rhs));
    failwith("LSP: EXN: Nonempty Rightwards segment not single Convex Grout");
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

let piece_to_left = (z: Zipper.t): thing_to_left =>
  /*
   Returning Left means we're looking for either a new thing
    that starts with a left-facing chevron, or a completion of the
    thing to the left, which has a left-side right-facing chevron.
    */
  switch (z.relatives.siblings |> fst |> List.rev) {
  | [] => Nothing
  | [lht, ..._] as seg =>
    if (Piece.is_secondary(lht)) {
      print_endline("LSP: Leftward piece is Secondary; trimming");
    };
    switch (Segment.trim_secondary(Left, seg)) {
    | [] =>
      failwith("LSP: EXN: Rightwards segment after trimming secondaries")
    | [lht, ..._] =>
      switch (lht) {
      | Tile({label: [], _}) => failwith("LSP: EXN: Tile with empty label")
      | Tile({label: [tok_to_left], id, _} as t) =>
        print_endline("LSP: Leftward piece is Monotile: " ++ tile_str(t));
        let g =
          switch (right_nib_dir(t)) {
          | Right => LeftConvex(Monotile(id, tok_to_left))
          | Left => LeftConcave(Monotile(id, tok_to_left))
          };
        Piece.is_secondary(lht) ? SpacesThen(g) : Just(g);
      | Tile(t) =>
        print_endline(
          "LSP: Leftward piece is "
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
        failwith("LSP: EXN: Leftward Grout " ++ Grout.show_shape(shape))
      | Secondary(_) =>
        failwith("LSP: EXN: Secondary after trimming secondaries")
      }
    };
  };

//TODO: better handling of concaves
type generation_options =
  | NewRightConvex(Id.t)
  | LeftCompletionOrNewRightConvex(Id.t, string, Id.t)
  | NewRightConcave(Id.t) //id here is iffy
  | LeftCompletionOrNewRightConcave(Id.t, string, Id.t); //snd id here is iffy

let generation_options = (z: Zipper.t) => {
  switch (piece_to_left(z), hole_to_right(z)) {
  | (Nothing, None) => failwith("LSP: EXN: Nothing to left or right")
  | (Just(LeftConcave(_)) | SpacesThen(LeftConcave(_)), None) =>
    failwith("LSP: EXN: Concave to left and nothing to right")
  | (Just(LeftConvex(_)) | SpacesThen(LeftConvex(_)), Some(_)) =>
    failwith("LSP: EXN: Convex to left and right")
  | (Nothing | Just(LeftConcave(Polytile(_))) | SpacesThen(_), Some(id)) =>
    NewRightConvex(id)
  | (Just(LeftConcave(Monotile(id, left_token))), Some(id_to_right)) =>
    LeftCompletionOrNewRightConvex(id, left_token, id_to_right)
  | (
      Just(LeftConvex(Polytile(id))) |
      SpacesThen(LeftConvex(Polytile(id) | Monotile(id, _))),
      None,
    ) =>
    //TODO: id here is weird
    NewRightConcave(id)
  | (Just(LeftConvex(Monotile(id, left_token))), None) =>
    //TODO: id here is weird
    LeftCompletionOrNewRightConcave(id, left_token, id)
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

let backpack_is_convex = ({strategy, _} as s: Suggestion.t) =>
  switch (strategy) {
  | Any(FromBackpack(shape)) when shape == Convex => Some(s)
  | _ => None
  };

let backpack_is_concave = ({strategy, _} as s: Suggestion.t) =>
  switch (strategy) {
  | Any(FromBackpack(shape)) when shape != Convex => Some(s)
  | _ => None
  };

let nu_sugs =
    (
      ~debug=true,
      ~shape: Nib.Shape.t,
      ~completion: option(string),
      z: Zipper.t,
      id: Id.t,
    )
    : option(list(string)) => {
  let db = s =>
    if (debug) {
      print_endline(s);
    };
  let settings = CoreSettings.on;
  let ctx_init = []; //Builtins.ctx_init;
  let* ci = lsp_z_to_ci(~settings, ~ctx=ctx_init, id, z);
  let sort = Info.sort_of(ci);
  let cls = Info.cls_of(ci);
  let ctx = Info.ctx_of(ci);
  let expected_ty = AssistantForms.Typ.expected(ci);
  let backpack_tokens = TyDi.backpack_to_token_list(z.backpack);
  let backpack_suggestion = suggest_backpack(z);
  let base_convex_mono_suggestions = AssistantForms.suggest_operand(ci);
  let abstract_mono_operand_suggestions =
    AssistantForms.suggest_abstract_mono(ci);
  let leading_suggestions = AssistantForms.suggest_leading(ci);
  let variable_suggestions = AssistantCtx.suggest_variable(ci);
  let lookahead_variable_suggestions =
    AssistantCtx.suggest_lookahead_variable(ci);
  let operator_suggestions = AssistantForms.suggest_operator(ci);
  db("LSP: Cursor info from form: " ++ Term.Cls.show(cls));
  db("LSP: Having sort: " ++ Sort.to_string(sort));
  db("LSP: Having ctx: " ++ Ctx.to_string(ctx));
  db("LSP: Having expected type: " ++ Typ.to_string(expected_ty));
  db("LSP: Having error: " ++ error_str(ci));
  db("LSP: Backpack stack: " ++ String.concat(" ", backpack_tokens));
  db("LSP: Base Backpack: " ++ of_sugs(backpack_suggestion));
  db("LSP: Base Convex Monos: " ++ of_sugs(base_convex_mono_suggestions));
  db(
    "LSP: Base Convex Abstract Monos: "
    ++ of_sugs(abstract_mono_operand_suggestions),
  );
  db("LSP: Base Leadings: " ++ of_sugs(leading_suggestions));
  db("LSP: Base Variables: " ++ of_sugs(variable_suggestions));
  db(
    "LSP: Base Lookahead Variables: "
    ++ of_sugs(lookahead_variable_suggestions),
  );
  db("LSP: Base Operator: " ++ of_sugs(operator_suggestions));
  //TODO(andrew): am i missing negation operator from convexes?
  //TODO(andrew): think through function application concavity
  //TODO(andrew): not all leading necessarily convex?
  let suggestions_convex =
    base_convex_mono_suggestions
    @ abstract_mono_operand_suggestions
    @ leading_suggestions
    @ variable_suggestions
    @ lookahead_variable_suggestions
    |> List.sort(Suggestion.compare);
  let suggestions_concave =
    AssistantForms.suggest_operator(ci) |> List.sort(Suggestion.compare);
  switch (completion) {
  | None =>
    let base_suggestions =
      switch (shape) {
      | Convex =>
        suggestions_convex
        @ List.filter_map(backpack_is_convex, backpack_suggestion)
      | Concave(_) =>
        suggestions_concave
        @ List.filter_map(backpack_is_concave, backpack_suggestion)
      };
    Some(List.map(Suggestion.content_of, base_suggestions));
  | Some(tok_to_left) =>
    /* Note that for completion suggestions, we don't want to case on
       shape expectation, because the tile might get remolded */
    let suggestions_all =
      suggestions_concave @ suggestions_convex @ backpack_suggestion;
    suggestions_all
    |> List.filter(({content, _}: Suggestion.t) =>
         String.starts_with(~prefix=tok_to_left, content)
       )
    |> List.map(Suggestion.content_of)
    |> List.filter_map(x => TyDi.suffix_of(x, tok_to_left))
    |> Option.some;
  };
};

let dispatch_generation = (s: string) => {
  let* z = Printer.zipper_of_string(s);
  print_endline("LSP: String parsed successfully");
  let seg_before = z.relatives.siblings |> fst |> List.rev;
  let seg_after = z.relatives.siblings |> snd;
  if (z.caret != Outer) {
    print_endline(
      "Segment before: "
      ++ Segment.show(seg_before)
      ++ "Segment after: "
      ++ Segment.show(seg_after),
    );
    failwith("LSP: EXN: Caret position is inner");
  };
  if (seg_before == [] && seg_after == []) {
    failwith("LSP: EXN: Empty segment");
  };
  switch (generation_options(z)) {
  | NewRightConvex(id) =>
    let sugs = nu_sugs(~shape=Convex, ~completion=None, z, id);
    print_endline("LSP: Dispatch: Can insert new right-convex");
    switch (sugs) {
    | None => print_endline("LSP: Suggestion generation failed")
    | Some(sugs) =>
      print_endline(
        "LSP: (1/1) New right-convex Suggestions: "
        ++ String.concat(" ", sugs),
      )
    };
    sugs;
  | NewRightConcave(id) =>
    print_endline("LSP: Dispatch: Can insert new right-concave");
    let sugs = nu_sugs(~shape=Concave(0), ~completion=None, z, id);
    switch (sugs) {
    | None => print_endline("LSP: Suggestion generation failed")
    | Some(sugs) =>
      print_endline(
        "LSP: (1/1) New right-concave Suggestions: "
        ++ String.concat(" ", sugs),
      )
    };
    sugs;
  | LeftCompletionOrNewRightConvex(id_left, string, id_right) =>
    print_endline(
      "LSP: Dispatch: Can insert new right-convex or complete left",
    );
    let* a = nu_sugs(~shape=Convex, ~completion=None, z, id_right);
    let+ b =
      nu_sugs(~shape=Concave(0), ~completion=Some(string), z, id_left);
    print_endline(
      "LSP: (1/2) New right-convex Suggestions: " ++ String.concat(" ", a),
    );
    print_endline(
      "LSP: (2/2) Completion Suggestions: " ++ String.concat(" ", b),
    );
    a @ b;
  | LeftCompletionOrNewRightConcave(id_left, string, id_right) =>
    print_endline(
      "LSP: Dispatch: Can insert new right-concave or complete left",
    );
    let* a = nu_sugs(~shape=Concave(0), ~completion=None, z, id_right);
    let+ b = nu_sugs(~shape=Convex, ~completion=Some(string), z, id_left);
    print_endline(
      "LSP: (1/2) New right-concave Suggestions: " ++ String.concat(" ", a),
    );
    print_endline(
      "LSP: (2/2) Completion Suggestions: " ++ String.concat(" ", b),
    );
    a @ b;
  };
};

let toks_to_grammar = (toks: list(string)): string => {
  //TODO: just let copilot generate these prefix patterns lol, better checkem
  let prefix = {|
intlit ::= [0-9]+
floatlit ::= [0-9]+ "." [0-9]+
stringlit ::= "\"" [^"]* "\""
patvar ::= [a-zA-Z_][a-zA-Z0-9_]*
typvar ::= [A-Z][a-zA-Z0-9_]*
whitespace ::= [ \t\n]+

root ::=
    whitespace
  | |};
  //TODO: make whitespace conditional on something
  let toks =
    List.map(
      tok =>
        switch (tok) {
        | "~INTLIT~" => "intlit"
        | "~FLOATLIT~" => "floatlit"
        | "~STRINGLIT~" => "stringlit"
        | "~PATVAR~" => "patvar"
        | "~TYPVAR~" => "typvar"
        | tok => "\"" ++ tok ++ "\""
        },
      toks,
    );
  let toks = String.concat("\n  | ", toks);
  prefix ++ toks;
};

let finals = (s: string) => {
  switch (dispatch_generation(s)) {
  | None => print_endline("LSP: No suggestions")
  | Some(sugs) => prerr_endline("LSP: Grammar:\n " ++ toks_to_grammar(sugs))
  };
};

finals(arg_str);

/*
TODO(andrew):
probably want to be more liberal about synthetic position,
inclding applying things of unknown type, or that return unknown type
 */
